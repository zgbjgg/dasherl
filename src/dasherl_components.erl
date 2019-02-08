% This module is a helper to build all components exposed by dash api
% as a single record (erlang term) so you can write erlang terms to build
% dash layouts.

-module(dasherl_components).

-include("dasherl_components.hrl").

% html components
-export([divc/1,
    h1/1, h2/1, h3/1, h4/1,
    p/1, label/1, table/1]).

% core components
-export([input/1,
    dropdown/1,
    slider/1,
    range_slider/1,
    textarea/1,
    check_list/1,
    radio_items/1,
    date_picker_single/1,
    date_picker_range/1,
    markdown/1,
    upload/1,
    tabs/1, tab/1,
    graph/1,
    confirm_dialog/1]).

% html

h1(Keywords) ->
    #dasherl_html_component{type='H1', children=Keywords}.

h2(Keywords) ->
    #dasherl_html_component{type='H2', children=Keywords}.

h3(Keywords) ->
    #dasherl_html_component{type='H3', children=Keywords}.

h4(Keywords) ->
    #dasherl_html_component{type='H4', children=Keywords}.

p(Keywords) ->
    #dasherl_html_component{type='P', children=Keywords}.

label(Keywords) ->
    #dasherl_html_component{type='Label', children=Keywords}.

table(Keywords) ->
    #dasherl_html_component{type='Table', children=Keywords}.

divc(Keywords) ->
    #dasherl_html_component{type='Div', children=Keywords}.

% core

input(Keywords) ->
    #dasherl_core_component{type='Input', children=Keywords}.

dropdown(Keywords) ->
    #dasherl_core_component{type='Dropdown', children=Keywords}.

slider(Keywords) ->
    #dasherl_core_component{type='Slider', children=Keywords}.

range_slider(Keywords) ->
    #dasherl_core_component{type='RangeSlider', children=Keywords}.

textarea(Keywords) ->
    #dasherl_core_component{type='Textarea', children=Keywords}.

check_list(Keywords) ->
    #dasherl_core_component{type='Checklist', children=Keywords}.

radio_items(Keywords) ->
    #dasherl_core_component{type='RadioItems', children=Keywords}.

date_picker_single(Keywords) ->
    #dasherl_core_component{type='DatePickerSingle', children=Keywords}.

date_picker_range(Keywords) ->
    #dasherl_core_component{type='DatePickerRange', children=Keywords}.

markdown(Keywords) ->
    #dasherl_core_component{type='Markdown', children=Keywords}.

input(Keywords) ->
    #dasherl_core_component{type='Input', children=Keywords}.

upload(Keywords) ->
    #dasherl_core_component{type='Upload', children=Keywords}.

input(Keywords) ->
    #dasherl_core_component{type='Input', children=Keywords}.

tabs(Keywords) ->
    #dasherl_core_component{type='Tabs', children=Keywords}.

tab(Keywords) ->
    #dasherl_core_component{type='Tab', children=Keywords}.

graph(Keywords) ->
    #dasherl_core_component{type='Graph', children=Keywords}.

confirm_dialog(Keywords) ->
    #dasherl_core_component{type='ConfirmDialog', children=Keywords}.
