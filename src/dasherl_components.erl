% This module is a helper to build all components exposed by dash api
% as a single record (erlang term) so you can write erlang terms to build
% dash layouts.

-module(dasherl_components).

-include("dasherl_components.hrl").

% html components
-export([divc/1]).

% core components
-export([input/1]).

divc(Keywords) ->
    #dasherl_html_component{type='Div', children=Keywords}.

input(Keywords) ->
    #dasherl_core_component{type='Input', children=Keywords}.
