-module(dasherl_flow_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_dasherl_setup_route/1,
    test_dasherl_del_route/1]).

all() ->
    [test_dasherl_setup_route,
     test_dasherl_del_route].

init_per_testcase(_, _Config) ->
    % use ibrowse
    _ = ibrowse:start(),
    ok = application:ensure_started(compiler),
    ok = application:ensure_started(syntax_tools),
    ok = application:ensure_started(goldrush),
    ok = application:ensure_started(lager),
    ok = application:ensure_started(erlport),
    ok = application:start(dasherl),
    {ok, Pid} = dasherl_gunicorn_worker:start_link([{bind, "127.0.0.1:8000"},
        {workers, 1}]),
    ok = dasherl_gunicorn_worker:run_server(Pid),
    [{worker, Pid}].

end_per_testcase(_, Config) ->
    Pid = proplists:get_value(worker, Config),
    ok = dasherl_gunicorn_worker:stop_server(Pid),
    ok = dasherl_gunicorn_worker:stop(Pid),
    ok = application:stop(dasherl),
    ok.

test_dasherl_setup_route(Config) ->
    Pid = proplists:get_value(worker, Config),

    % define a custom layout from erlang
    Input = dasherl_components:input([{placeholder, 'Write here'}, {value, ''}, {type, 'text'}]),
    Div = dasherl_components:divc([{children, [Input]}]),

    % set router to render layout
    ok = dasherl_gunicorn_worker:setup_route(Pid, <<"/my-dasherl-route">>, Div),

    % use ibrowse to check
    {ok, Code, _, _} = ibrowse:send_req("http://127.0.0.1:8000/my-dasherl-route", [], get),

    ?assertEqual("200", Code).

test_dasherl_del_route(Config) ->
    Pid = proplists:get_value(worker, Config),

    % define a custom layout from erlang
    Input = dasherl_components:input([{placeholder, 'Write here'}, {value, ''}, {type, 'text'}]),
    Div = dasherl_components:divc([{children, [Input]}]),

    % set router to render layout
    ok = dasherl_gunicorn_worker:setup_route(Pid, <<"/my-dasherl-route">>, Div),

    % remove route
    ok = dasherl_gunicorn_worker:del_route(Pid, <<"/my-dasherl-route">>),

    % use ibrowse to check
    {ok, Code, _, _} = ibrowse:send_req("http://127.0.0.1:8000/my-dasherl-route", [], get),

    ?assertEqual("200", Code).
