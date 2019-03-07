-module(mod_helper).

-behaviour(dasherl_handler).

-export([layout/0, callbacks/0, update_output_div/2]).

layout() ->
    Input = dasherl_components:input([{value, 'initial value'}, {type, 'text'}, {id, 'my-id'}]),
    Div = dasherl_components:divc([{id, 'my-div'}]),
    dasherl_components:divc([{children, [Input, Div]}]).

callbacks() ->
    Output = dasherl_dependencies:output('my-div', 'children'),
    Input = dasherl_dependencies:input('my-id', 'value'),
    [{Output, [Input], 'update_output_div', ?MODULE, update_output_div}].

update_output_div(_Bind, {InputValue}) ->
    Retval = "You've entered " ++ InputValue,
    list_to_atom(Retval).
