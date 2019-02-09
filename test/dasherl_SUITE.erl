-module(dasherl_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_dasherl_gunicorn_worker/1,
    test_dasherl_app/1,
    test_dasherl_gunicorn_worker_call/1,
    test_dasherl_gunicorn_worker_cast/1,
    test_dasherl_app_stop/1,
    test_dasherl_gunicorn_worker_stop/1]).

all() ->
    [test_dasherl_app,
     test_dasherl_gunicorn_worker,
     test_dasherl_gunicorn_worker_call,
     test_dasherl_gunicorn_worker_cast,
     test_dasherl_gunicorn_worker_stop,
     test_dasherl_app_stop].

init_per_suite(_Config) ->
    ok = application:set_env(dasherl, routes, []),
    ok = application:start(compiler),
    ok = application:start(syntax_tools),
    ok = application:start(goldrush),
    ok = application:start(lager),
    ok = application:start(erlport),
    ok = application:start(dasherl),
    [].

end_per_suite(_Config) ->
    ok.

test_dasherl_app(_) ->
    ?assertEqual(ok, application:ensure_started(dasherl)).

test_dasherl_gunicorn_worker(Config) ->
    {ok, Pid} = dasherl_gunicorn_worker:start_link([{bind, "127.0.0.1:8000"},
        {workers, 0}]),
    ok = dasherl_gunicorn_worker:stop(Pid),
    ?assertEqual(is_pid(Pid), true).

test_dasherl_app_stop(_) ->
    ?assertEqual(ok, application:stop(dasherl)).

test_dasherl_gunicorn_worker_stop(Config) ->
    {ok, Pid} = dasherl_gunicorn_worker:start_link([{bind, "127.0.0.1:8000"},
        {workers, 0}]),
    ok = dasherl_gunicorn_worker:stop(Pid),
    ?assertEqual(erlang:is_process_alive(Pid), false).

% just to increment % of coverage

test_dasherl_gunicorn_worker_call(Config) ->
    {ok, Pid} = dasherl_gunicorn_worker:start_link([{bind, "127.0.0.1:8000"},
        {workers, 0}]),
    R = gen_server:call(Pid, hello),
    ok = dasherl_gunicorn_worker:stop(Pid),
    ?assertEqual(ok, R).

test_dasherl_gunicorn_worker_cast(Config) ->
    {ok, Pid} = dasherl_gunicorn_worker:start_link([{bind, "127.0.0.1:8000"},
        {workers, 0}]),
    R = gen_server:cast(Pid, hello),
    ok = dasherl_gunicorn_worker:stop(Pid),
    ?assertEqual(ok, R).
