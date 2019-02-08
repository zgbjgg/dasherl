-module(dasherl_gunicorn_worker).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).

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

stop(Pid) ->
    gen_server:call(Pid, stop_link).

init(Settings) ->
    process_flag(trap_exit, true),
    lager:debug("starting gunicorn worker with settings: ~p", [Settings]),

    % actually settings can be set from config but also can be passed as an
    % argument of init gen_server
    Workers = proplists:get_value(workers, Settings, ?DEFAULT_WORKERS),
    Bind = proplists:get_value(bind, Settings, ?DEFAULT_BIND),

    Path = code:priv_dir(dasherl),

    % start the py process and initializes its importing modules
    case python:start([{python_path, Path}]) of
        {ok, PyPid} ->
            MonRef = erlang:monitor(process, PyPid),
            lager:info("initialized default modules for py pid ~p", [PyPid]),

            % initialize decoder for erl dash components
            ok = python:call(PyPid, dasherl_components, setup_dasherl_components_type, []),

            % initialize gunicorn with dasherl and hold in a process
            PidGunicorn = initialize_from_scratch(PyPid, Workers, Bind),
            lager:info("gunicorn is up and running at linked process: ~p", [PidGunicorn]),

            {ok, #state{py_pid = PyPid, mon_ref = MonRef, workers = Workers,
                bind = Bind, gunicorn_pid = PidGunicorn}};
        Error      ->
            lager:error("cannot initializes py due to ~p", [Error]),
            {stop, Error}
    end.

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

% since calling to gunicorn from shell is a blocking call, just
% call into a spawn process, so we can handle separately (in async mode)
% and follow the entire gen_server process
initialize_from_scratch(PyPid, Workers, Bind) ->
    BindAtom = list_to_atom(Bind),
    % for now use a static stylesheet
    Stylesheet = ['https://codepen.io/chriddyp/pen/bWLwgP.css'],
    spawn_link(python, call, [PyPid, dasherl, initialize,
        [Workers, BindAtom, Stylesheet]]).

% since is a blocking process, gunicorn cannot be stopped just with py
% so stop process linked, after that stop with signal handler using
% sigterm.
stop_signal(Pid) ->
    exit(Pid, kill),
    [UnixPid|_] = string:split(os:cmd("cat " ++ ?DEFAULT_UNIX_PID), "\n"),
    _ = os:cmd("kill -9 " ++ UnixPid),
    ok.
