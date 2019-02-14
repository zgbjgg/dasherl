-module(dasherl_gunicorn_worker).

-behaviour(gen_server).

%% API
-export([start_link/1,
    stop/1,
    run_server/1,
    stop_server/1,
    setup_route/3,
    del_route/2,
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
    bind = "127.0.0.1:80" :: string()}).

start_link(Settings) ->
    gen_server:start_link(?MODULE, Settings, []).

stop(Pid) ->
    gen_server:call(Pid, stop_link).

run_server(Pid) ->
    gen_server:call(Pid, run_server).

stop_server(Pid) ->
    gen_server:call(Pid, stop_server).

setup_route(Pid, Route, Layout) ->
    gen_server:call(Pid, {setup_route, Route, Layout}).

del_route(Pid, Route) ->
    gen_server:call(Pid, {del_route, Route}).

setup_callback(Pid, Outputs, Inputs, Lambda) ->
    gen_server:call(Pid, {setup_callback, Outputs, Inputs, Lambda}).

init(Settings) ->
    process_flag(trap_exit, true),
    lager:debug("starting gunicorn worker with settings: ~p", [Settings]),

    % actually settings can be set from config but also can be passed as an
    % argument of init gen_server
    Workers = proplists:get_value(workers, Settings, ?DEFAULT_WORKERS),
    Bind = proplists:get_value(bind, Settings, ?DEFAULT_BIND),
    Stylesheets = proplists:get_value(stylesheets, Settings,
        ['https://codepen.io/chriddyp/pen/bWLwgP.css']),

    Path = code:priv_dir(dasherl),

    % start the py process and initializes its importing modules
    case python:start([{python_path, Path}]) of
        {ok, PyPid} ->
            MonRef = erlang:monitor(process, PyPid),
            lager:info("initialized default modules for py pid ~p", [PyPid]),

            % initialize decoder for erl dash components
            ok = python:call(PyPid, dasherl_components, setup_dasherl_components_type, []),

            "ok" = python:call(PyPid, dasherl, initialize, [Workers, list_to_atom(Bind),
                Stylesheets, list_to_binary(pid_to_list(PyPid))]),

            {ok, #state{py_pid = PyPid, mon_ref = MonRef, workers = Workers,
                bind = Bind}};
        Error      ->
            lager:error("cannot initializes py due to ~p", [Error]),
            {stop, Error}
    end.

handle_call(run_server, _From, State) ->
    PyPid = State#state.py_pid,
    Appid = list_to_binary(pid_to_list(PyPid)),
    Pid = list_to_binary(pid_to_list(self())),
    case catch python:call(PyPid, dasherl, run, [Appid, Pid]) of
        {'EXIT', {{python, Class, Argument, _Stack}, _}} ->
            {reply, {error, {Class, Argument}}, State};
        "ok"                                             ->
            {reply, ok, State}
    end;
handle_call(stop_server, _From, State) ->
    PyPid = State#state.py_pid,
    Pid = list_to_binary(pid_to_list(self())),
    case catch python:call(PyPid, dasherl, stop, [Pid]) of
        {'EXIT', {{python, Class, Argument, _Stack}, _}} ->
            {reply, {error, {Class, Argument}}, State};
        "no_proc"                                        ->
            {reply, {error, no_proc}, State};
        "ok"                                             ->
            {reply, ok, State}
    end;
handle_call({setup_route, Route, Layout}, _From, State) ->
    PyPid = State#state.py_pid,
    case catch python:call(PyPid, dasherl, setup_route, [Route, Layout]) of
        {'EXIT', {{python, Class, Argument, _Stack}, _}} ->
            {reply, {error, {Class, Argument}}, State};
        "ok"                                             ->
            {reply, ok, State}
    end;
handle_call({del_route, Route}, _From, State) ->
    PyPid = State#state.py_pid,
    case catch python:call(PyPid, dasherl, del_route, [Route]) of
        {'EXIT', {{python, Class, Argument, _Stack}, _}} ->
            {reply, {error, {Class, Argument}}, State};
        "ok"                                             ->
            {reply, ok, State}
    end;
handle_call({setup_callback, Outputs, Inputs, Lambda}, _From, State) ->
    PyPid = State#state.py_pid,
    case catch python:call(PyPid, dasherl, setup_callback, [Outputs, Inputs, Lambda]) of
        {'EXIT', {{python, Class, Argument, _Stack}, _}} ->
            {reply, {error, {Class, Argument}}, State};
        "ok"                                             ->
            {reply, ok, State}
    end;
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
    % when finish process just stop py_pid
    ok = python:stop(State#state.py_pid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================
%% Internal Funcionts
%% ===================================
