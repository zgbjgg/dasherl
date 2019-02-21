-module(dasherl).

-export([compile/3]).

% the compilation process is to setup the layout, callbacks
% and all the needed environment for erlang module works. The erlang
% module to be available to compile should implement the behaviour
% of dasherl base behaviour
compile(Server, Path, Mod) when is_pid(Server) ->
    % first to all setup routes into router using layout from $mod
    Layout = erlang:apply(Mod, layout, []),
    case dasherl_worker:compile_layout(Layout) of
        {ok, CompiledLayout} ->
            ok = dasherl_router:add_route(Path, CompiledLayout);
        _Error               ->
            throw(bad_or_malformed_layout)
    end,

    % next setup the callbacks ...
    case erlang:apply(Mod, callbacks, []) of
        Deps when is_list(Deps) ->
            ok = lists:foreach(fun({Outputs, Inputs, Bind, Handler, Fun}) ->
                % compile all outputs and inputs
                CompiledOutputs = compile_dep(Outputs),
                CompiledInputs = compile_dep(Inputs),
                case dasherl_gunicorn_worker:setup_callback(Server, CompiledOutputs, CompiledInputs, Bind) of
                    ok ->
                        % now setup the binding function
                        ok = dasherl_binding:add_callback(Bind, Handler, Fun);
                    _  ->
                        throw(cannot_setup_callbacks)
                end
            end, Deps);
        _                                          ->
            throw(bad_or_malformed_callbacks)
    end;
compile(_, _, _) ->
    throw(server_should_be_a_pid).

% compile each dep and check if its single or a list of deps
compile_dep([]) -> [];
compile_dep(Dep) when is_record(Dep, dasherl_output_dependency, 3) ->
    {ok, CompiledDep} = dasherl_worker:compile_dependency(Dep),
    CompiledDep;
compile_dep(Dep) when is_record(Dep, dasherl_input_dependency, 3)  ->
    {ok, CompiledDep} = dasherl_worker:compile_dependency(Dep),
    CompiledDep;
compile_dep([Dep | Deps]) ->
    [ compile_dep(Dep) | compile_dep(Deps) ];
compile_dep(_) -> [].
