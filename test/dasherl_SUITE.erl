-module(dasherl_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test_dasherl_gunicorn_worker/1,
    test_dasherl_app/1,
    test_dasherl_gunicorn_worker_call/1,
    test_dasherl_gunicorn_worker_cast/1,
    test_dasherl_app_stop/1,
    test_dasherl_gunicorn_worker_run_server/1]).

all() ->
    [test_dasherl_app,
     test_dasherl_gunicorn_worker,
     test_dasherl_gunicorn_worker_call,
     test_dasherl_gunicorn_worker_cast,
     test_dasherl_gunicorn_worker_run_server,
     test_dasherl_app_stop].

init_per_testcase(_, _Config) ->
    % start a dasherl gunicorn worker
    ok = application:set_env(dasherl, routes, []),
    {ok, Pid} = dasherl_gunicorn_worker:start_link([{bind, "127.0.0.1:8000"},
        {workers, 1}]),
    [{worker, Pid}].

end_per_testcase(_, Config) ->
    Pid = proplists:get_value(worker, Config),
    ok = case erlang:is_process_alive(Pid) of
        true  -> dasherl_gunicorn_worker:stop(Pid);
        false -> ok
    end,
    ok.

test_dasherl_app(_) ->
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(goldrush),
    ok = application:start(lager),
    ok = application:start(erlport),
    ok = application:start(dasherl),
    ?assertEqual(ok, application:ensure_started(dasherl)).

test_dasherl_gunicorn_worker(Config) ->
    Pid = proplists:get_value(worker, Config),
    ?assertEqual(erlang:is_process_alive(Pid), true).

test_dasherl_app_stop(_) ->
    ?assertEqual(ok, application:stop(dasherl)).

test_dasherl_gunicorn_worker_run_server(Config) ->
    Pid = proplists:get_value(worker, Config),
    ?assertEqual(ok, dasherl_gunicorn_worker:run_server(Pid)).

% just to increment % of coverage

test_dasherl_gunicorn_worker_call(Config) ->
    Pid = proplists:get_value(worker, Config),
    R = gen_server:call(Pid, hello),
    ?assertEqual(ok, R).

test_dasherl_gunicorn_worker_cast(Config) ->
    Pid = proplists:get_value(worker, Config),
    R = gen_server:cast(Pid, hello),
    ?assertEqual(ok, R).
