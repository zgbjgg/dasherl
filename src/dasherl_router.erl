-module(dasherl_router).

-behaviour(gen_server).

%% API
-export([start_link/0,
    stop/0,
    render/1,
    add_route/2,
    remove_route/1,
    list/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

% the values can be override during initialization
-record(state, {routes :: list()}).

start_link() ->
    {ok, Routes} = application:get_env(dasherl, routes),
    gen_server:start_link({local, ?MODULE}, ?MODULE, Routes, []).

stop() ->
    gen_server:call(?MODULE, stop_link).

render(Path) ->
    gen_server:call(?MODULE, {render, Path}).

add_route(Path, Layout) ->
    gen_server:call(?MODULE, {add, Path, Layout}).

remove_route(Path) ->
    gen_server:call(?MODULE, {remove, Path}).

list() ->
    gen_server:call(?MODULE, list).

init(Routes) ->
    process_flag(trap_exit, true),
    lager:debug("starting router with routes: ~p", [Routes]),
    {ok, #state{routes = Routes}}.

handle_call(stop_link, _From, State) ->
    {stop, normal, ok, State};

handle_call({render, Path}, _From, State) ->
    % since a plist, just pick with proplists
    PyLayout = case lists:keyfind(Path, 1, State#state.routes) of
        {_Path, Layout} -> Layout;
        false           -> no_such_layout
    end,
    {reply, PyLayout, State};

handle_call({add, Path, Layout}, _From, State=#state{routes = Routes}) ->
    {reply, ok, State#state{routes = Routes ++ [{Path, Layout}]}};

handle_call({remove, Path}, _From, State=#state{routes = Routes}) ->
    NewRoutes = case lists:keyfind(Path, 1, Routes) of
        false -> Routes;
        Route -> Routes -- [Route]
    end,
    {reply, ok, State#state{routes = NewRoutes}};

handle_call(list, _From, State=#state{routes = Routes}) ->
    {reply, {ok, Routes}, State};

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
