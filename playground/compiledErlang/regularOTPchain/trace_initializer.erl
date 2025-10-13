-module(trace_initializer).
-export([init/0, stop/0]).

init() ->
    TracerPid = my_tracer:start(),
    
    %% Start the application processes
    worker_b:start_link(),
    worker_a:start_link(),
    central:start_link(),

    %% Get PIDs
    ServerPid = whereis(central),
    A = whereis(worker_a),
    B = whereis(worker_b),

    %% Enable tracing on selected processes
    erlang:trace(ServerPid, true, [call, send, 'receive', {tracer, TracerPid}]),
    erlang:trace(A, true, [call, send, 'receive', {tracer, TracerPid}]),
    erlang:trace(B, true, [call, send, 'receive', {tracer, TracerPid}]),

    %% Trace local function calls with return info
    erlang:trace_pattern({central, '_', '_'}, [{'_', [], [{return_trace}]}], [local]),
    erlang:trace_pattern({worker_a, '_', '_'}, [{'_', [], [{return_trace}]}], [local]),
    erlang:trace_pattern({worker_b, '_', '_'}, [{'_', [], [{return_trace}]}], [local]),

    io:format("Tracing initialized with tracer ~p~n", [TracerPid]),
    {ok, TracerPid}.

stop() ->
    erlang:trace(all, false, [call, send, 'receive']),
    io:format("Tracing stopped~n").
