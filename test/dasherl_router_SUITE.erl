-module(dasherl_router_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_dasherl_router_add/1,
    test_dasherl_router_remove/1,
    test_dasherl_router_list/1]).

all() ->
    [test_dasherl_router_add,
     test_dasherl_router_remove,
     test_dasherl_router_list].

init_per_suite(_Config) ->
    ok = application:set_env(dasherl, routes, [{"/route1", clayout}]),
    ok = application:start(dasherl),
    [].

end_per_suite(_Config) ->
    ok = application:stop(dasherl),
    ok.

test_dasherl_router_add(_) ->
    ok = dasherl_router:add_route("/new", new),
    ?assertEqual(new, dasherl_router:render("/new")).

test_dasherl_router_remove(_) ->
    ok = dasherl_router:remove_route("/new"),
    ?assertEqual(no_such_layout, dasherl_router:render("/new")).

test_dasherl_router_list(_) ->
    {ok, Routes} = dasherl_router:list(),
    ?assertEqual([{"/route1", clayout}], Routes).
