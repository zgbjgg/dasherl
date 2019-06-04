-module(dasherl_components_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_dasherl_divc/1,
    test_dasherl_h1/1,
    test_dasherl_h2/1,
    test_dasherl_h3/1,
    test_dasherl_h4/1,
    test_dasherl_p/1,
    test_dasherl_label/1,
    test_dasherl_table/1]).

-export([test_dasherl_input/1,
    test_dasherl_dropdown/1,
    test_dasherl_slider/1,
    test_dasherl_range_slider/1,
    test_dasherl_textarea/1,
    test_dasherl_check_list/1,
    test_dasherl_radio_items/1,
    test_dasherl_date_picker_single/1,
    test_dasherl_date_picker_range/1,
    test_dasherl_markdown/1,
    test_dasherl_upload/1,
    test_dasherl_tabs/1,
    test_dasherl_tab/1,
    test_dasherl_graph/1,
    test_dasherl_confirm_dialog/1,
    test_dasherl_datatable/1,
    test_dasherl_interval/1]).

all() ->
    [test_dasherl_divc,
     test_dasherl_h1,
     test_dasherl_h2,
     test_dasherl_h3,
     test_dasherl_h4,
     test_dasherl_p,
     test_dasherl_label,
     test_dasherl_table,
     test_dasherl_input,
     test_dasherl_dropdown,
     test_dasherl_slider,
     test_dasherl_range_slider,
     test_dasherl_textarea,
     test_dasherl_check_list,
     test_dasherl_radio_items,
     test_dasherl_date_picker_single,
     test_dasherl_date_picker_range,
     test_dasherl_markdown,
     test_dasherl_upload,
     test_dasherl_tabs,
     test_dasherl_tab,
     test_dasherl_graph,
     test_dasherl_confirm_dialog,
     test_dasherl_datatable,
     test_dasherl_interval].

init_per_suite(_Config) ->
    [].

end_per_suite(_Config) ->
    ok.

test_dasherl_divc(_) ->
    Div = dasherl_components:divc([]), 
    ?assertEqual({dasherl_html_component, 'Div', []}, Div).

test_dasherl_h1(_) ->
    H1 = dasherl_components:h1([]),
    ?assertEqual({dasherl_html_component, 'H1', []}, H1).

test_dasherl_h2(_) ->
    H2 = dasherl_components:h2([]),
    ?assertEqual({dasherl_html_component, 'H2', []}, H2).

test_dasherl_h3(_) ->
    H3 = dasherl_components:h3([]),
    ?assertEqual({dasherl_html_component, 'H3', []}, H3).

test_dasherl_h4(_) ->
    H4 = dasherl_components:h4([]),
    ?assertEqual({dasherl_html_component, 'H4', []}, H4).

test_dasherl_p(_) ->
    P = dasherl_components:p([]),
    ?assertEqual({dasherl_html_component, 'P', []}, P).

test_dasherl_label(_) ->
    Label = dasherl_components:label([]),
    ?assertEqual({dasherl_html_component, 'Label', []}, Label).

test_dasherl_table(_) ->
    Table = dasherl_components:table([]),
    ?assertEqual({dasherl_html_component, 'Table', []}, Table).

test_dasherl_input(_) ->
    Input = dasherl_components:input([]),
    ?assertEqual({dasherl_core_component, 'Input', []}, Input).

test_dasherl_dropdown(_) ->
    Dropdown = dasherl_components:dropdown([]),
    ?assertEqual({dasherl_core_component, 'Dropdown', []}, Dropdown).

test_dasherl_slider(_) ->
    Slider = dasherl_components:slider([]),
    ?assertEqual({dasherl_core_component, 'Slider', []}, Slider).

test_dasherl_range_slider(_) ->
    RangeSlider = dasherl_components:range_slider([]),
    ?assertEqual({dasherl_core_component, 'RangeSlider', []}, RangeSlider).

test_dasherl_textarea(_) ->
    Textarea = dasherl_components:textarea([]),
    ?assertEqual({dasherl_core_component, 'Textarea', []}, Textarea).

test_dasherl_check_list(_) ->
    Checklist = dasherl_components:check_list([]),
    ?assertEqual({dasherl_core_component, 'Checklist', []}, Checklist).

test_dasherl_radio_items(_) ->
    RadioItems = dasherl_components:radio_items([]),
    ?assertEqual({dasherl_core_component, 'RadioItems', []}, RadioItems).

test_dasherl_date_picker_single(_) ->
    DatePickerSingle = dasherl_components:date_picker_single([]),
    ?assertEqual({dasherl_core_component, 'DatePickerSingle', []}, DatePickerSingle).

test_dasherl_date_picker_range(_) ->
    DatePickerRange = dasherl_components:date_picker_range([]),
    ?assertEqual({dasherl_core_component, 'DatePickerRange', []}, DatePickerRange).

test_dasherl_markdown(_) ->
    Markdown = dasherl_components:markdown([]),
    ?assertEqual({dasherl_core_component, 'Markdown', []}, Markdown).

test_dasherl_upload(_) ->
    Upload = dasherl_components:upload([]),
    ?assertEqual({dasherl_core_component, 'Upload', []}, Upload).

test_dasherl_tabs(_) ->
    Tabs = dasherl_components:tabs([]),
    ?assertEqual({dasherl_core_component, 'Tabs', []}, Tabs).

test_dasherl_tab(_) ->
    Tab = dasherl_components:tab([]),
    ?assertEqual({dasherl_core_component, 'Tab', []}, Tab).

test_dasherl_graph(_) ->
    Graph = dasherl_components:graph([]),
    ?assertEqual({dasherl_core_component, 'Graph', []}, Graph).

test_dasherl_confirm_dialog(_) ->
    ConfirmDialog = dasherl_components:confirm_dialog([]),
    ?assertEqual({dasherl_core_component, 'ConfirmDialog', []}, ConfirmDialog).

test_dasherl_datatable(_) ->
    DataTable = dasherl_components:datatable([]),
    ?assertEqual({dasherl_core_component, 'DataTable', []}, DataTable).

test_dasherl_interval(_) ->
    Interval = dasherl_components:interval([]),
    ?assertEqual({dasherl_core_component, 'Interval', []}, Interval).
