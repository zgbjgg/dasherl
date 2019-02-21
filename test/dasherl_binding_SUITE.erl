-module(dasherl_binding_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_dasherl_binding_add_callback/1,
    test_dasherl_binding_callback/1,
    test_dasherl_binding_callback_error/1,
    test_dasherl_binding_list/1]).

all() ->
    [test_dasherl_binding_add_callback,
     test_dasherl_binding_callback,
     test_dasherl_binding_callback_error,
     test_dasherl_binding_list].

init_per_suite(_Config) ->
    ok = application:set_env(dasherl, routes, []),
    ok = application:start(dasherl),
    [].

end_per_suite(_Config) ->
    ok = application:stop(dasherl),
    ok.

test_dasherl_binding_add_callback(_) ->
    ok = dasherl_binding:add_callback(bind, mod_helper, update_output_div),
    ?assertEqual(list_to_atom("You've entered 1"), dasherl_binding:callback(bind, {"1"})).

test_dasherl_binding_callback(_) ->
    ?assertEqual(list_to_atom("You've entered 1"), dasherl_binding:callback(bind, {"1"})).

test_dasherl_binding_callback_error(_) ->
    ?assertEqual(error_in_response, dasherl_binding:callback(bind_none, {"x"})).

test_dasherl_binding_list(_) ->
    {ok, Callbacks} = dasherl_binding:list(),
    ?assertEqual([{bind, {mod_helper, update_output_div}}], Callbacks).
