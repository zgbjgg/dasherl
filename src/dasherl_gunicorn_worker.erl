-module(dasherl_gunicorn_worker).

-behaviour(gen_server).

%% API
-export([start_link/1,
    stop/1,
    run_server/1,
    setup_callback/4]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-include("dasherl.hrl").

% the values can be override during initialization
-record(state, {py_pid = undefined :: pid(),
    mon_ref = undefined,
    workers = 3 :: non_neg_integer(),
    bind = "127.0.0.1:80" :: string(),
    gunicorn_pid = undefined :: pid()}).

start_link(Settings) ->
    gen_server:start_link(?MODULE, Settings, []).

run_server(Pid) ->
    gen_server:call(Pid, run_server).

stop(Pid) ->
    gen_server:call(Pid, stop_link).

setup_callback(Pid, Outputs, Inputs, Bind) ->
    gen_server:call(Pid, {setup_callback, Outputs, Inputs, Bind}).

init(Settings) ->
    process_flag(trap_exit, true),
    lager:debug("starting gunicorn worker with settings: ~p", [Settings]),

    % actually settings can be set from config but also can be passed as an
    % argument of init gen_server
    Workers = proplists:get_value(workers, Settings, ?DEFAULT_WORKERS),
    Bind = proplists:get_value(bind, Settings, ?DEFAULT_BIND),
    Stylesheets = proplists:get_value(stylesheets, Settings, ?DEFAULT_STYLESHEETS),

    Path = code:priv_dir(dasherl),

    % start the py process and initializes its importing modules
    case python:start([{python_path, Path}]) of
        {ok, PyPid} ->
            MonRef = erlang:monitor(process, PyPid),

            % initialize gunicorn with dasherl and hold in a process
            PidGunicorn = initialize_from_scratch(PyPid, Workers, Bind, Stylesheets),
            lager:info("gunicorn is up and running at linked process: ~p", [PidGunicorn]),

            {ok, #state{py_pid = PyPid, mon_ref = MonRef, workers = Workers,
                bind = Bind, gunicorn_pid = PidGunicorn}};
        Error      ->
            lager:error("cannot initializes py due to ~p", [Error]),
            {stop, Error}
    end.

handle_call({setup_callback, Outputs, Inputs, Bind}, _From, State) ->
    % @WARNING this only should work when gunicorn is not running, so setup MUST be called
    % before run server.
    PyPid = State#state.py_pid,
   case catch python:call(PyPid, dasherl, setup_callback, [Outputs, Inputs, Bind]) of
        {'EXIT', {{python, Class, Argument, _Stack}, _}} ->
            {reply, {error, {Class, Argument}}, State};
        "ok"                                             ->
            {reply, ok, State}
    end;
handle_call(run_server, _From, State) ->
    PyPid = State#state.py_pid,
    Appid = list_to_binary(pid_to_list(self())),

    % from python side this call blocks the entire port, so hold in a separate
    % process and keep this gen_server normal state
    PidGunicorn = spawn_link(python, call, [PyPid, dasherl, run, [Appid]]),

    {reply, ok, State#state{gunicorn_pid = PidGunicorn}};
handle_call(stop_link, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->    
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', MonRef, _Type, _Object, _Info}, State=#state{mon_ref = MonRef}) ->
    % process py pid is down, which one is the process to restart?
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ok = stop_signal(State#state.gunicorn_pid),
    % when finish process just stop py_pid
    ok = python:stop(State#state.py_pid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================
%% Internal Funcionts
%% ===================================

% initialize the app and keep it in python side so we
% can run after the entire gen_server was initialized correctly 
initialize_from_scratch(PyPid, Workers, Bind, Stylesheets) ->
    BindAtom = list_to_atom(Bind),
    Appid = list_to_binary(pid_to_list(self())),
    case catch python:call(PyPid, dasherl, initialize, [Workers, BindAtom, Stylesheets, Appid]) of
        {'EXIT', {{python, Class, Argument, _Stack}, _}} ->
            {error, {Class, Argument}};
        "ok"                                             ->
            ok
    end.

% since is a blocking process, gunicorn cannot be stopped just with py
% so stop process linked, after that stop with signal handler using
% sigterm.
stop_signal(Pid) ->
    exit(Pid, kill),
    [UnixPid|_] = string:tokens(os:cmd("cat " ++ ?DEFAULT_UNIX_PID), "\n"),
    _ = os:cmd("kill -9 " ++ UnixPid),
    ok.
