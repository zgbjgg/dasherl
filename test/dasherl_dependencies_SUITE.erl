-module(dasherl_dependencies_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_dasherl_output/1,
    test_dasherl_input/1]).

all() ->
    [test_dasherl_output,
     test_dasherl_input].

init_per_suite(_Config) ->
    [].

end_per_suite(_Config) ->
    ok.

test_dasherl_input(_) ->
    Input = dasherl_dependencies:input(input, value),
    ?assertEqual({dasherl_input_dependency, input, value}, Input).

test_dasherl_output(_) ->
    Output = dasherl_dependencies:output(output, children),
    ?assertEqual({dasherl_output_dependency, output, children}, Output).
