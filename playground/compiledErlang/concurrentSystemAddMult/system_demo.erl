%% system_demo.erl
-module(system_demo).
-export([start/0, start_with_tracing/0, stop/0, demo_flow/0]).

start() ->
    io:format("=== Starting Concurrent Processing System ===~n"),
    
    %% Start the central server (which starts proc_add10 and proc_mul2)
    case slow_central_server:start_link() of
        {ok, Pid} ->
            io:format("Central server started with PID: ~p~n", [Pid]),
            io:format("System ready! You can now run concurrent_test:run()~n"),
            ok;
        {error, Reason} ->
            io:format("Failed to start central server: ~p~n", [Reason]),
            error
    end.

start_with_tracing() ->
    io:format("=== Starting System with Tracing ===~n"),
    
    %% Initialize tracing first
    case trace_initializer:init() of
        {ok, TracerPid} ->
            io:format("Tracer started with PID: ~p~n", [TracerPid]),
            
            %% Start the system
            case start() of
                ok ->
                    io:format("System started with tracing enabled~n"),
                    io:format("Run concurrent_test:run() to see traced execution~n"),
                    ok;
                error ->
                    trace_initializer:stop(),
                    error
            end;
        Error ->
            io:format("Failed to start tracer: ~p~n", [Error]),
            error
    end.

stop() ->
    io:format("=== Stopping System ===~n"),
    
    %% Stop tracing if it was enabled
    trace_initializer:stop(),
    
    %% Stop all registered processes
    lists:foreach(fun(Name) ->
        case whereis(Name) of
            undefined -> ok;
            Pid ->
                io:format("Stopping ~p (~p)~n", [Name, Pid]),
                exit(Pid, shutdown)
        end
    end, [slow_central_server, slow_proc_add10, slow_proc_mul2]),
    
    %% Clean up any remaining client processes
    lists:foreach(fun(Name) ->
        case whereis(Name) of
            undefined -> ok;
            Pid when is_atom(Name) ->
                NameStr = atom_to_list(Name),
                case lists:prefix("client", NameStr) of
                    true ->
                        io:format("Stopping client ~p (~p)~n", [Name, Pid]),
                        exit(Pid, shutdown);
                    false -> ok
                end
        end
    end, registered()),
    
    io:format("System stopped~n"),
    ok.

%% Helper function to demonstrate the expected flow
demo_flow() ->
    io:format("=== Expected Flow Demonstration ===~n"),
    io:format("1. Client sends number N to central_server~n"),
    io:format("2. central_server spawns worker and forwards to proc_add10~n"),
    io:format("3. proc_add10 spawns worker, adds 10, forwards to proc_mul2~n"),
    io:format("4. proc_mul2 spawns worker, multiplies by 2, returns result~n"),
    io:format("5. Result flows back: mul2 -> add10 -> central -> client~n"),
    io:format("Expected result for input N: (N + 10) * 2~n"),
    io:format("Example: input 5 -> (5 + 10) * 2 = 30~n").
