-module(dasherl_binding).

-behaviour(gen_server).

%% API
-export([start_link/0,
    stop/0,
    callback/2,
    add_callback/3,
    list/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

% the values can be override during initialization
-record(state, {callbacks :: list()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop_link).

callback(Bind, Args) ->
    gen_server:call(?MODULE, {callback, Bind, Args}).

add_callback(Bind, Mod, Fun) ->
    gen_server:call(?MODULE, {add, Bind, Mod, Fun}).

list() ->
    gen_server:call(?MODULE, list).

init(Callbacks) ->
    process_flag(trap_exit, true),
    lager:debug("starting binding for callbacks: ~p", [Callbacks]),
    {ok, #state{callbacks = Callbacks}}.

handle_call(stop_link, _From, State) ->
    {stop, normal, ok, State};

handle_call({callback, Bind, Args}, _From, State) ->
    % since a plist, just pick with proplists
    Response = case lists:keyfind(Bind, 1, State#state.callbacks) of
        {Bind, {Mod, Fun}} -> erlang:apply(Mod, Fun, [Bind, Args]);
        false               -> error_in_response
    end,
    {reply, Response, State};

handle_call({add, Bind, Mod, Fun}, _From, State=#state{callbacks = Callbacks}) ->
    {reply, ok, State#state{callbacks = Callbacks ++ [{Bind, {Mod, Fun}}]}};

handle_call(list, _From, State=#state{callbacks = Callbacks}) ->
    {reply, {ok, Callbacks}, State};

handle_call(_Request, _From, State) ->    
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================
%% Internal Funcionts
%% ===================================
