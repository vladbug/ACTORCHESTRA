-module(trace_initializer).
-export([init/0, stop/0]).

init() ->
    TracerPid = my_tracer:start(),

    wrapper:start_link(),
    worker_b:start_link(),
    worker_a:start_link(),

    %% Trace only specific processes (can expand if needed)
    lists:foreach(fun(P) ->
        erlang:trace(P, true, [call, send, 'receive', {tracer, TracerPid}])
    end, [
        whereis(worker_a),
        whereis(worker_b),
        whereis(wrapper)
    ]),

    %% Function-level return traces
    erlang:trace_pattern({worker_a, '_', '_'}, [{'_', [], [{return_trace}]}], [local]),
    erlang:trace_pattern({worker_b, '_', '_'}, [{'_', [], [{return_trace}]}], [local]),

    io:format("Tracing initialized with wrapper and tracer ~p~n", [TracerPid]),
    ok.

stop() ->
    erlang:trace(all, false, [call, send, 'receive']),
    my_custom_tracer ! {stop},
    io:format("Tracing stopped and log closed~n").
