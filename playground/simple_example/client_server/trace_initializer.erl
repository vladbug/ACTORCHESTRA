-module(trace_initializer).
-export([init/0, stop/0]).

init() -> 
    TracerPid = my_tracer:start(),
    ServerPid = central_server:start(),
    wait_for_process(incrementer),

    %% Setup trace
    erlang:trace(ServerPid, true, [call, send, 'receive', {tracer, TracerPid}]),
    erlang:trace(whereis(incrementer), true, [call, send, 'receive', {tracer, TracerPid}]),

    erlang:trace_pattern({central_server, '_', '_'}, [{'_', [], [{return_trace}]}], [local]),
    erlang:trace_pattern({incrementer, '_', '_'}, [{'_', [], [{return_trace}]}], [local]),

    io:format("Tracing initialized with custom tracer ~p~n", [TracerPid]),
    {ok, TracerPid, ServerPid}.

stop() ->
    erlang:trace(all, false, [call, send, 'receive']),
    io:format("Tracing disabled~n").

wait_for_process(Name) ->
    case whereis(Name) of
        undefined ->
            timer:sleep(100),
            wait_for_process(Name);
        Pid when is_pid(Pid) ->
            ok
    end.

