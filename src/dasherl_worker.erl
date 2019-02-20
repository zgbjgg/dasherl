-module(dasherl_worker).

-behaviour(gen_server).

%% API
-export([start_link/0,
    stop/0,
    compile_layout/1,
    compile_dependency/1]).

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
    mon_ref = undefined}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop_link).

compile_layout(Layout) ->
    gen_server:call(?MODULE, {compile_layout, Layout}).

compile_dependency(Dep) ->
    gen_server:call(?MODULE, {compile_dep, Dep}).

init([]) ->
    process_flag(trap_exit, true),

    Path = code:priv_dir(dasherl),

    % start the py process and initializes its importing modules
    case python:start([{python_path, Path}]) of
        {ok, PyPid} ->
            MonRef = erlang:monitor(process, PyPid),
            lager:info("initialized default modules for py pid ~p", [PyPid]),

            % initialize decoder for erl dash components
            ok = python:call(PyPid, dasherl_components, setup_dasherl_components_type, []),

            {ok, #state{py_pid = PyPid, mon_ref = MonRef}};
        Error      ->
            lager:error("cannot initializes py due to ~p", [Error]),
            {stop, Error}
    end.

handle_call({compile_layout, Layout}, _From, State) ->
    PyPid = State#state.py_pid,
    case catch python:call(PyPid, dasherl_components, compile, [Layout]) of
        {'EXIT', {{python, Class, Argument, _Stack}, _}} ->
            {reply, {error, {Class, Argument}}, State};
        PyLayout                                         ->
            {reply, PyLayout, State}
    end;
handle_call({compile_dep, Dep}, _From, State) ->
    PyPid = State#state.py_pid,
    case catch python:call(PyPid, dasherl_components, compile, [Dep]) of
        {'EXIT', {{python, Class, Argument, _Stack}, _}} ->
            {reply, {error, {Class, Argument}}, State};
        PyDep                                            ->
            {reply, PyDep, State}
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
