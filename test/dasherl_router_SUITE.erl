-module(dasherl_router_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_dasherl_router_add/1,
    test_dasherl_router_remove/1,
    test_dasherl_router_list/1,
    test_dasherl_router_render_preloaded/1,
    test_dasherl_router_render_none/1,
    test_dasherl_router_render_erl/1]).

all() ->
    [test_dasherl_router_add,
     test_dasherl_router_remove,
     test_dasherl_router_list,
     test_dasherl_router_render_preloaded,
     test_dasherl_router_render_none,
     test_dasherl_router_render_erl].

init_per_suite(_Config) ->
    ok = application:set_env(dasherl, routes, [{"/preloaded", preloaded},
        {"/erl", {erl, {}}}]),
    ok = application:start(dasherl),
    [].

end_per_suite(_Config) ->
    ok = application:stop(dasherl),
    ok.

test_dasherl_router_add(_) ->
    ok = dasherl_router:add_route("/new", new),
    ?assertEqual({new, true}, dasherl_router:render("/new")).

test_dasherl_router_remove(_) ->
    ok = dasherl_router:remove_route("/new"),
    ?assertEqual({none, false}, dasherl_router:render("/new")).

test_dasherl_router_list(_) ->
    {ok, Routes} = dasherl_router:list(),
    ?assertEqual([{"/preloaded", preloaded}, {"/erl", {erl, {}}}], Routes).

test_dasherl_router_render_preloaded(_) ->
    ?assertEqual({preloaded, true}, dasherl_router:render("/preloaded")).

test_dasherl_router_render_none(_) ->
    ?assertEqual({none, false}, dasherl_router:render("/none")).

test_dasherl_router_render_erl(_) ->
    ?assertEqual({{}, false}, dasherl_router:render("/erl")).
