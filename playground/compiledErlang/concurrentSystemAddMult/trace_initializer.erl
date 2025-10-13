%% trace_initializer.erl
-module(trace_initializer).
-export([init/0, stop/0]).

init() ->
    Tracer = my_tracer:start(),

    %% 1) Trace all processes for calls, sends, and receives
    erlang:trace(all, true, [call, send, 'receive', {tracer, Tracer}]),

    %% 2) Add return‐trace patterns for only our relevant modules
    erlang:trace_pattern({central_server, '_', '_'},
                        [{'_', [], [{return_trace}]}], [local]),
    erlang:trace_pattern({proc_add10, '_', '_'},
                        [{'_', [], [{return_trace}]}], [local]),
    erlang:trace_pattern({proc_mul2, '_', '_'},
                        [{'_', [], [{return_trace}]}], [local]),
    erlang:trace_pattern({client, '_', '_'},
                        [{'_', [], [{return_trace}]}], [local]),

    io:format("Tracing initialized with custom tracer ~p~n", [Tracer]),
    {ok, Tracer}.

stop() ->
    %% Turn off all tracing
    erlang:trace(all, false, [call, send, 'receive']),

    %% Clear our trace patterns
    erlang:trace_pattern({central_server, '_', '_'}, false),
    erlang:trace_pattern({proc_add10,    '_', '_'}, false),
    erlang:trace_pattern({proc_mul2,     '_', '_'}, false),
    erlang:trace_pattern({client,        '_', '_'}, false),

    io:format("Tracing disabled~n"),
    ok.

