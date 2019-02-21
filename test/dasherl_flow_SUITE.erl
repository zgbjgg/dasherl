-module(dasherl_flow_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_dasherl_compile_mod/1]).

all() ->
    [test_dasherl_compile_mod].

init_per_testcase(_, _Config) ->
    % use ibrowse
    _ = ibrowse:start(),
    ok = application:ensure_started(compiler),
    ok = application:ensure_started(syntax_tools),
    ok = application:ensure_started(goldrush),
    ok = application:ensure_started(lager),
    ok = application:ensure_started(erlport),
    ok = application:start(dasherl),
    ok = application:set_env(dasherl, routes, []),
    {ok, Pid} = dasherl_gunicorn_worker:start_link([{bind, "127.0.0.1:8000"},
        {workers, 1}]),
    [{worker, Pid}].

end_per_testcase(_, Config) ->
    Pid = proplists:get_value(worker, Config),
    ok = dasherl_gunicorn_worker:stop(Pid),
    ok = application:stop(dasherl),
    ok.

test_dasherl_compile_mod(Config) ->
    Pid = proplists:get_value(worker, Config),

    % define a custom module and compile using dasherl
    ok = dasherl:compile(Pid, "/my-dasherl-route", mod_helper),

    ok = dasherl_gunicorn_worker:run_server(Pid),

    % use ibrowse to check
    {ok, Code, _, _} = ibrowse:send_req("http://127.0.0.1:8000/my-dasherl-route", [], get),

    ?assertEqual("200", Code).
