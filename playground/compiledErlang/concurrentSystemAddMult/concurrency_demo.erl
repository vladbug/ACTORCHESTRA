-module(concurrency_demo).
-export([demo_fast/0, demo_slow/0, compare_sequential_vs_concurrent/0, stop_all/0]).

%% Demo with fast processors (hard to see concurrency)
demo_fast() ->
    io:format("=== FAST PROCESSOR DEMO ===~n"),
    io:format("Processing is very fast, concurrency may be hard to see~n~n"),
    
    stop_all(),
    
    %% Start fast system
    case system_demo:start_with_tracing() of
        ok ->
            io:format("Fast system started~n"),
            timer:sleep(500),
            
            %% Run burst test
            stress_test:run_burst(),
            timer:sleep(1000),
            
            %% Run simultaneous test  
            stress_test:run_simultaneous(),
            
            io:format("~n=== FAST DEMO COMPLETE ===~n")
    end.

%% Demo with overlapping requests (easy to see concurrency)
demo_slow() ->
    io:format("=== OVERLAPPING REQUESTS DEMO ===~n"),
    io:format("Multiple clients sending overlapping requests to show concurrency~n~n"),
    
    stop_all(),
    
    %% Start normal system
    case system_demo:start_with_tracing() of
        ok ->
            io:format("System started~n"),
            timer:sleep(500),
            
            io:format("~n--- Starting 5 clients with overlapping requests ---~n"),
            
            %% Start 5 clients that will send overlapping requests with small delays
            spawn(fun() -> overlapping_client_runner(1, [1, 2, 3], 0) end),
            spawn(fun() -> overlapping_client_runner(2, [10, 11, 12], 50) end),   % Start 50ms later
            spawn(fun() -> overlapping_client_runner(3, [20, 21, 22], 100) end),  % Start 100ms later
            spawn(fun() -> overlapping_client_runner(4, [30, 31, 32], 150) end),  % Start 150ms later
            spawn(fun() -> overlapping_client_runner(5, [40, 41, 42], 200) end),  % Start 200ms later
            
            %% Let them run and show the overlap
            timer:sleep(5000),
            
            io:format("~n=== OVERLAPPING DEMO COMPLETE ===~n"),
            io:format("You should see multiple workers processing different numbers simultaneously!~n")
    end.

%% Compare sequential vs concurrent processing times
compare_sequential_vs_concurrent() ->
    io:format("=== PERFORMANCE COMPARISON ===~n"),
    
    stop_all(),
    timer:sleep(1000),  % Give more time for cleanup
    
    %% Test 1: Sequential processing (if we had no concurrency)
    io:format("~n--- Simulating Sequential Processing ---~n"),
    Requests = [1, 2, 3, 4, 5],
    
    SeqStart = erlang:system_time(millisecond),
    lists:foreach(fun(N) ->
        % Simulate processing time: each request takes some time
        timer:sleep(100),  % Simulate processing delay
        Result = (N + 10) * 2,
        io:format("Sequential: ~p -> ~p~n", [N, Result])
    end, Requests),
    SeqEnd = erlang:system_time(millisecond),
    SeqTime = SeqEnd - SeqStart,
    
    io:format("Sequential total time: ~p ms~n", [SeqTime]),
    
    %% Test 2: Concurrent processing
    io:format("~n--- Testing Concurrent Processing ---~n"),
    case system_demo:start() of
        ok ->
            timer:sleep(500),
            
            %% Create a single client for all requests
            {ok, _} = client:start_link(perf_client),
            timer:sleep(100),
            ok = client:connect(perf_client),
            
            ConcStart = erlang:system_time(millisecond),
            
            %% Send all requests simultaneously using the client properly
            RequestPids = lists:map(fun(N) ->
                spawn(fun() ->
                    Result = client:send_number(perf_client, N),
                    io:format("Concurrent: ~p -> ~p~n", [N, Result])
                end)
            end, Requests),
            
            %% Wait for all to complete
            lists:foreach(fun(Pid) ->
                monitor(process, Pid),
                receive
                    {'DOWN', _, process, Pid, _} -> ok
                after 5000 ->
                    io:format("Timeout waiting for request~n")
                end
            end, RequestPids),
            
            ConcEnd = erlang:system_time(millisecond),
            ConcTime = ConcEnd - ConcStart,
            
            client:disconnect(perf_client),
            
            io:format("~nConcurrent total time: ~p ms~n", [ConcTime]),
            io:format("~n=== PERFORMANCE RESULTS ===~n"),
            io:format("Sequential: ~p ms~n", [SeqTime]),
            io:format("Concurrent: ~p ms~n", [ConcTime]),
            
            if ConcTime > 0 ->
                io:format("Speedup: ~.2fx faster~n", [SeqTime / ConcTime]),
                io:format("Efficiency: ~.1f%~n", [SeqTime * 100 / ConcTime / length(Requests)]);
            true ->
                io:format("Concurrent processing was too fast to measure accurately~n")
            end;
        error ->
            io:format("Failed to start system for concurrent test~n")
    end.

%% Helper functions
overlapping_client_runner(ClientNum, Numbers, StartDelay) ->
    timer:sleep(StartDelay),
    
    ClientName = list_to_atom("overlap_client" ++ integer_to_list(ClientNum)),
    {ok, _} = client:start_link(ClientName),
    timer:sleep(50),  % Small delay to ensure client is ready
    ok = client:connect(ClientName),
    
    StartTime = erlang:system_time(millisecond),
    io:format("[~p ms] === Client~p STARTING ===~n", [StartTime, ClientNum]),
    
    %% Send requests with no delay between them to create overlapping processing
    RequestPids = lists:map(fun(N) ->
        spawn(fun() ->
            ReqStart = erlang:system_time(millisecond),
            io:format("[~p ms] Client~p: Sending request ~p~n", [ReqStart, ClientNum, N]),
            
            Result = client:send_number(ClientName, N),
            
            ReqEnd = erlang:system_time(millisecond),
            ReqTime = ReqEnd - ReqStart,
            io:format("[~p ms] Client~p: Got ~p -> ~p (took ~p ms)~n", 
                     [ReqEnd, ClientNum, N, Result, ReqTime])
        end)
    end, Numbers),
    
    %% Wait for all requests from this client to complete
    lists:foreach(fun(Pid) ->
        monitor(process, Pid),
        receive
            {'DOWN', _, process, Pid, _} -> ok
        after 5000 ->
            io:format("Timeout waiting for client ~p request~n", [ClientNum])
        end
    end, RequestPids),
    
    client:disconnect(ClientName),
    EndTime = erlang:system_time(millisecond),
    TotalTime = EndTime - StartTime,
    io:format("[~p ms] === Client~p FINISHED (total: ~p ms) ===~n", 
             [EndTime, ClientNum, TotalTime]).

stop_all() ->
    io:format("Stopping all systems...~n"),
    
    %% Stop tracing first
    try trace_initializer:stop() catch _:_ -> ok end,
    timer:sleep(100),
    
    %% Get all registered processes before we start killing them
    RegisteredProcesses = registered(),
    
    %% Stop all known server processes
    ServerNames = [central_server, proc_add10, proc_mul2, 
                   slow_central_server, slow_proc_add10, slow_proc_mul2],
    lists:foreach(fun(Name) ->
        case whereis(Name) of
            undefined -> ok;
            Pid -> 
                io:format("Stopping ~p (~p)~n", [Name, Pid]),
                exit(Pid, kill),  % Use kill instead of shutdown for more aggressive cleanup
                timer:sleep(50)
        end
    end, ServerNames),
    
    %% Clean up all client processes
    lists:foreach(fun(Name) ->
        case whereis(Name) of
            undefined -> ok;
            Pid when is_atom(Name) ->
                NameStr = atom_to_list(Name),
                IsClientProcess = lists:prefix("client", NameStr) orelse 
                                lists:prefix("overlap_client", NameStr) orelse
                                lists:prefix("burst_client", NameStr) orelse
                                lists:prefix("delayed_client", NameStr) orelse
                                lists:prefix("perf_client", NameStr) orelse
                                lists:prefix("test_client", NameStr),
                case IsClientProcess of
                    true -> 
                        io:format("Stopping client ~p (~p)~n", [Name, Pid]),
                        exit(Pid, kill);
                    false -> ok
                end;
            _ -> ok
        end
    end, RegisteredProcesses),
    
    %% Wait for cleanup
    timer:sleep(500),
    
    %% Verify cleanup
    RemainingProcs = lists:filter(fun(Name) ->
        case whereis(Name) of
            undefined -> false;
            _ -> 
                NameStr = atom_to_list(Name),
                lists:prefix("client", NameStr) orelse 
                lists:member(Name, ServerNames)
        end
    end, registered()),
    
    case RemainingProcs of
        [] -> io:format("All systems stopped cleanly~n");
        _ -> io:format("Warning: Some processes still running: ~p~n", [RemainingProcs])
    end.