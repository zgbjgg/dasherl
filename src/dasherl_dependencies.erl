% This module is a helper to build all dash dependencies used into a 
% callback decorator as a single record (erlang term).

-module(dasherl_dependencies).

-include("dasherl_dependencies.hrl").

% core
-export([input/2, output/2]).

input(Component, Event) ->
    #dasherl_input_dependency{component=Component, event=Event}.

output(Component, Event) ->
    #dasherl_output_dependency{component=Component, event=Event}.
