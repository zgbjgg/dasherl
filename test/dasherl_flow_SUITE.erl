-module(dasherl_flow_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_dasherl/1]).

all() ->
    [test_dasherl].

init_per_suite(_Config) ->
    ok = application:set_env(dasherl, routes, []),
    ok = application:start(dasherl),
    % maybe pid is stale
    [UnixPid|_] = string:split(os:cmd("cat /tmp/dasherl_gunicorn.pid"), "\n"),
    _ = os:cmd("kill -9 " ++ UnixPid), 
    [].

end_per_suite(_Config) ->
    ok = application:stop(dasherl),
    ok.

test_dasherl(_) ->
    % define a custom layout from erlang
    Input = dasherl_components:input([{placeholder, 'Write here'}, {value, ''}, {type, 'text'}]),
    Div = dasherl_components:divc([{children, [Input]}]),

    % set router to render layout
    ok = dasherl_router:add_route("/my-dasherl-route", {erl, Div}),

    {ok, Pid} = dasherl_gunicorn_worker:start_link([{bind, "127.0.0.1:8000"},
        {workers, 1}]),

    timer:sleep(5000),

    % use ibrowse to check
    {ok, _} = ibrowse:start(),
    {ok, Code, _, _} = ibrowse:send_req("http://127.0.0.1:8000/my-dasherl-route", [], get),

    % stop worker
    ok = dasherl_gunicorn_worker:stop(Pid),

    ?assertEqual("200", Code).
