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
-record(state, {workers = 3 :: non_neg_integer(),
    bind = "127.0.0.1:80" :: string(),    
    unix_pid = "/tmp/dasherl_gunicorn.pid" :: string(),
    port = undefined,
    os_pid :: non_neg_integer()}).

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

    % the chdir is the priv directory of root dasherl app
    Chdir = code:priv_dir(dasherl),

    % config the pid file
    UnixPid = proplists:get_value(unix_pid, Settings, "/tmp/dasherl_gunicorn.pid"),

    % parse arguments and build a full cmd to start gunicorn
    {Cmd, Args} = build_cmd(UnixPid, Workers, Bind, Chdir),
    lager:info("starting gunicorn ~p ~p", [Cmd, Args]),
    Port = open_port({spawn, Cmd ++ lists:append(Args)},
        [exit_status, use_stdio, stderr_to_stdout]),
    Pid = case erlang:port_info(Port) of
        undefined ->
            lager:warning("error to start gunicorn, actually down!", []),
            undefined;
        PInfo     ->
            lager:info("gunicorn is up and running, info ~p", [PInfo]),
            proplists:get_value(os_pid, PInfo)
    end,
    {ok, #state{ workers = Workers,
        bind = Bind,
        unix_pid = UnixPid,
        port = Port,
        os_pid = Pid}}.

handle_call(stop_link, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->    
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % ensure terminate os gunicorn process
    ok = kill_cmd(State#state.os_pid, State#state.port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================
%% Internal Funcionts
%% ===================================

% @hidden
% build cmd for running gunicorn
build_cmd(UnixPid, Workers, Bind, Chdir) ->
    Executable = case os:find_executable(?GUNICORN) of
        false -> undefined;
        Value -> Value
    end,
    {Executable, [?BIND(Bind), ?WORKERS(Workers),
        ?CHDIR(Chdir), ?PIDFILE(UnixPid), ?APP]}.

% @hidden
% kill gunicorn os pid and port
kill_cmd(OSPid, Port) ->
    lager:debug("trying to kill gunicorn server instance at ~p", [OSPid]),
    case OSPid of
        undefined -> ok; % nothing to be done
        OSPid     ->
            os:cmd("kill -TERM " ++ integer_to_list(OSPid)),
            catch port_close(Port),
            ok
    end.
