-module(manual_instrumented_test).
-export([
    run_test/0, 
    run_simple_test/0, 
    run_complex_test/0, 
    cleanup/0, 
    test_quick/0, 
    test_batch/0, 
    test_concurrent/0, 
    test_batch_averaged/0,
    % New multi-client concurrent test functions
    run_multi_client_test/2,
    test_multi_client_averaged/0,
    test_multi_client_averaged/2,
    test_multi_client_quick/0
]).

%% Simple manual test - single operation
run_simple_test() ->
    io:format("=== MANUAL INSTRUMENTED TEST - SINGLE OPERATION ===~n"),
    
    %% Setup instrumented system
    {ok, _} = monitor:start(),
    {ok, _} = simple_wrapper:start_link(),
    {ok, _} = slow_central_server:start_link(),
    {ok, _} = client:start_link(test_client),
    ok = gen_server:call(test_client, connect),
    
    %% Single timed operation
    io:format("Executing single operation...~n"),
    StartTime = erlang:system_time(microsecond),
    
    Result = gen_server:call(test_client, {send_number, 42}, 10000),
    
    EndTime = erlang:system_time(microsecond),
    ElapsedTime = EndTime - StartTime,
    
    %% Results
    io:format("Result: ~p~n", [Result]),
    io:format("Time: ~p microseconds (~.2f milliseconds)~n", [ElapsedTime, ElapsedTime/1000]),
    
    %% Cleanup
    gen_server:call(test_client, disconnect),
    cleanup(),
    
    ElapsedTime.

%% Complex manual test - multiple operations to create substantial workload
run_complex_test() ->
    io:format("=== MANUAL INSTRUMENTED TEST - COMPLEX WORKLOAD ===~n"),
    
    %% Setup instrumented system
    {ok, _} = monitor:start(),
    {ok, _} = simple_wrapper:start_link(),
    {ok, _} = slow_central_server:start_link(),
    {ok, _} = client:start_link(test_client),
    ok = gen_server:call(test_client, connect),
    
    %% Complex workload - 100 operations
    NumberOfOperations = 100000,
    io:format("Executing ~p operations...~n", [NumberOfOperations]),
    
    StartTime = erlang:system_time(microsecond),
    
    %% Execute operations sequentially
    Results = lists:map(fun(N) ->
        gen_server:call(test_client, {send_number, N rem 50}, 10000)
    end, lists:seq(1, NumberOfOperations)),
    
    EndTime = erlang:system_time(microsecond),
    ElapsedTime = EndTime - StartTime,
    
    %% Results
    SuccessCount = length([R || R <- Results, R =/= error]),
    AvgTimePerOp = ElapsedTime / NumberOfOperations,
    
    io:format("Completed: ~p/~p operations successful~n", [SuccessCount, NumberOfOperations]),
    io:format("Total time: ~p microseconds (~.2f milliseconds)~n", [ElapsedTime, ElapsedTime/1000]),
    io:format("Average per operation: ~.2f microseconds (~.3f milliseconds)~n", 
             [AvgTimePerOp, AvgTimePerOp/1000]),
    io:format("Throughput: ~.2f operations/second~n", [NumberOfOperations / (ElapsedTime / 1000000)]),
    
    %% Show first few results as sample
    io:format("Sample results: ~p~n", [lists:sublist(Results, 5)]),
    
    %% Cleanup
    gen_server:call(test_client, disconnect),
    cleanup(),
    
    #{
        total_time_us => ElapsedTime,
        operations => NumberOfOperations,
        success_count => SuccessCount,
        avg_time_per_op_us => AvgTimePerOp,
        throughput_ops_per_sec => NumberOfOperations / (ElapsedTime / 1000000)
    }.

%% Default test runner - uses complex test
run_test() ->
    run_complex_test().

%% Concurrent test - multiple operations at once
run_concurrent_test() ->
    io:format("=== MANUAL INSTRUMENTED TEST - CONCURRENT WORKLOAD ===~n"),
    
    %% Setup instrumented system
    {ok, _} = monitor:start(),
    {ok, _} = simple_wrapper:start_link(),
    {ok, _} = slow_central_server:start_link(),
    {ok, _} = client:start_link(test_client),
    ok = gen_server:call(test_client, connect),
    
    %% Concurrent workload
    NumberOfOperations = 50,
    io:format("Executing ~p concurrent operations...~n", [NumberOfOperations]),
    
    StartTime = erlang:system_time(microsecond),
    ParentPid = self(),
    
    %% Launch all operations concurrently
    lists:foreach(fun(N) ->
        spawn(fun() ->
            OpStart = erlang:system_time(microsecond),
            Result = gen_server:call(test_client, {send_number, N rem 25}, 10000),
            OpEnd = erlang:system_time(microsecond),
            ParentPid ! {completed, N, Result, OpEnd - OpStart}
        end)
    end, lists:seq(1, NumberOfOperations)),
    
    %% Collect results
    Results = collect_concurrent_results(NumberOfOperations, []),
    
    EndTime = erlang:system_time(microsecond),
    TotalElapsedTime = EndTime - StartTime,
    
    %% Analyze results
    {Successes, OpTimes} = lists:foldl(fun({_N, Result, OpTime}, {SAcc, TAcc}) ->
        case Result of
            error -> {SAcc, TAcc};
            _ -> {SAcc + 1, [OpTime | TAcc]}
        end
    end, {0, []}, Results),
    
    AvgOpTime = case OpTimes of
        [] -> 0.0;
        _ -> lists:sum(OpTimes) / length(OpTimes)
    end,
    
    MinOpTime = case OpTimes of
        [] -> 0.0;
        _ -> lists:min(OpTimes)
    end,
    
    MaxOpTime = case OpTimes of
        [] -> 0.0;
        _ -> lists:max(OpTimes)
    end,
    
    %% Results
    io:format("Completed: ~p/~p operations successful~n", [Successes, NumberOfOperations]),
    io:format("Total time: ~p microseconds (~.2f milliseconds)~n", 
             [TotalElapsedTime, TotalElapsedTime/1000]),
    io:format("Operation times - Avg: ~.2f μs, Min: ~.2f μs, Max: ~.2f μs~n", 
             [AvgOpTime, MinOpTime, MaxOpTime]),
    io:format("Throughput: ~.2f operations/second~n", 
             [Successes / (TotalElapsedTime / 1000000)]),
    
    %% Cleanup
    gen_server:call(test_client, disconnect),
    cleanup(),
    
    #{
        total_time_us => TotalElapsedTime,
        operations => NumberOfOperations,
        success_count => Successes,
        avg_op_time_us => AvgOpTime,
        min_op_time_us => MinOpTime,
        max_op_time_us => MaxOpTime,
        throughput_ops_per_sec => Successes / (TotalElapsedTime / 1000000)
    }.

%% Helper to collect concurrent results
collect_concurrent_results(0, Acc) ->
    Acc;
collect_concurrent_results(Remaining, Acc) ->
    receive
        {completed, N, Result, OpTime} ->
            collect_concurrent_results(Remaining - 1, [{N, Result, OpTime} | Acc])
    after 30000 ->
        io:format("TIMEOUT: Still waiting for ~p results~n", [Remaining]),
        Acc
    end.

%% Cleanup function
cleanup() ->
    ProcessesToStop = [
        test_client,
        slow_central_server,
        slow_proc_add10,
        slow_proc_mul2,
        simple_wrapper
    ],
    
    lists:foreach(fun(Name) ->
        case whereis(Name) of
            undefined -> ok;
            Pid when is_pid(Pid) ->
                try
                    gen_server:stop(Name, normal, 1000)
                catch
                    _:_ -> 
                        try
                            exit(Pid, kill)
                        catch
                            _:_ -> ok
                        end
                end
        end
    end, ProcessesToStop),
    
    %% Stop monitor last
    catch monitor:stop(),
    
    timer:sleep(100),
    erlang:garbage_collect().

%% Quick test functions for manual execution
test_quick() ->
    io:format("Quick instrumented test:~n"),
    Time = run_simple_test(),
    io:format("~n=== RESULT: ~p microseconds ===~n", [Time]).

test_batch() ->
    io:format("Batch instrumented test:~n"),
    Result = run_complex_test(),
    io:format("~n=== RESULT: ~.2f ms total, ~.2f μs per operation ===~n", 
             [maps:get(total_time_us, Result)/1000, maps:get(avg_time_per_op_us, Result)]).

test_batch_averaged() ->
    io:format("Batch instrumented test (20 iterations):~n"),
    
    NumIterations = 20,
    WarmupIterations = 3,
    
    %% Warmup runs (discarded)
    io:format("Running ~p warmup iterations...~n", [WarmupIterations]),
    lists:foreach(fun(_) ->
        run_complex_test(),
        timer:sleep(100)  % Brief pause between runs
    end, lists:seq(1, WarmupIterations)),
    
    %% Actual measurement runs
    io:format("Running ~p measurement iterations...~n", [NumIterations]),
    Results = lists:map(fun(I) ->
        if I rem 5 == 0 -> io:format("  Completed ~p/~p...~n", [I, NumIterations]); 
           true -> ok 
        end,
        Result = run_complex_test(),
        timer:sleep(100),  % Brief pause between runs
        Result
    end, lists:seq(1, NumIterations)),
    
    %% Calculate averages
    TotalTimes = [maps:get(total_time_us, R) || R <- Results],
    AvgTimesPerOp = [maps:get(avg_time_per_op_us, R) || R <- Results],
    Throughputs = [maps:get(throughput_ops_per_sec, R) || R <- Results],
    
    AvgTotalTime = lists:sum(TotalTimes) / length(TotalTimes),
    AvgTimePerOp = lists:sum(AvgTimesPerOp) / length(AvgTimesPerOp),
    AvgThroughput = lists:sum(Throughputs) / length(Throughputs),
    
    %% Calculate standard deviations
    StdDevTotalTime = std_dev(TotalTimes, AvgTotalTime),
    StdDevTimePerOp = std_dev(AvgTimesPerOp, AvgTimePerOp),
    StdDevThroughput = std_dev(Throughputs, AvgThroughput),
    
    %% Display results
    io:format("~n=== AVERAGED RESULTS (n=~p) ===~n", [NumIterations]),
    io:format("Total Time:       ~.2f ± ~.2f ms~n", [AvgTotalTime/1000, StdDevTotalTime/1000]),
    io:format("Time per Operation: ~.2f ± ~.2f μs~n", [AvgTimePerOp, StdDevTimePerOp]),
    io:format("Throughput:       ~.2f ± ~.2f ops/sec~n", [AvgThroughput, StdDevThroughput]),
    
    %% Return summary map
    #{
        iterations => NumIterations,
        avg_total_time_us => AvgTotalTime,
        stddev_total_time_us => StdDevTotalTime,
        avg_time_per_op_us => AvgTimePerOp,
        stddev_time_per_op_us => StdDevTimePerOp,
        avg_throughput_ops_per_sec => AvgThroughput,
        stddev_throughput_ops_per_sec => StdDevThroughput,
        raw_results => Results
    }.

%% Helper function to calculate standard deviation
std_dev([], _) -> 0.0;
std_dev([_], _) -> 0.0;
std_dev(List, Mean) ->
    Variance = lists:sum([math:pow(X - Mean, 2) || X <- List]) / (length(List) - 1),
    math:sqrt(Variance).

test_concurrent() ->
    io:format("Concurrent instrumented test:~n"),
    Result = run_concurrent_test(),
    io:format("~n=== RESULT: ~.2f ms total, ~.2f μs avg per operation ===~n", 
             [maps:get(total_time_us, Result)/1000, maps:get(avg_op_time_us, Result)]).

%% Multi-client concurrent test - spawns separate clients
run_multi_client_test(NumClients, OperationsPerClient) ->
    io:format("=== MULTI-CLIENT INSTRUMENTED TEST - ~p CLIENTS, ~p OPS EACH ===~n", 
              [NumClients, OperationsPerClient]),
    
    %% Setup instrumented system
    {ok, _} = monitor:start(),
    {ok, _} = simple_wrapper:start_link(),
    {ok, _} = slow_central_server:start_link(),
    
    %% Create coordinator to collect results
    Coordinator = self(),
    
    %% Spawn client processes
    ClientPids = lists:map(fun(ClientId) ->
        spawn_link(fun() -> 
            concurrent_instrumented_client_worker(ClientId, OperationsPerClient, Coordinator) 
        end)
    end, lists:seq(1, NumClients)),
    
    io:format("Starting ~p concurrent instrumented clients...~n", [NumClients]),
    StartTime = erlang:system_time(microsecond),
    
    %% Send start signal to all clients
    lists:foreach(fun(Pid) -> Pid ! start end, ClientPids),
    
    %% Collect results from all clients
    Results = collect_multi_client_results(NumClients, []),
    
    EndTime = erlang:system_time(microsecond),
    TotalElapsedTime = EndTime - StartTime,
    
    %% Analyze results
    TotalOperations = NumClients * OperationsPerClient,
    AllClientResults = lists:flatten([ClientOps || {_ClientId, ClientOps} <- Results]),
    SuccessCount = length([R || R <- AllClientResults, R =/= error]),
    
    AvgTimePerOp = TotalElapsedTime / TotalOperations,
    ThroughputOpsPerSec = TotalOperations / (TotalElapsedTime / 1000000),
    
    %% Display results
    io:format("Completed: ~p/~p operations successful across ~p clients~n", 
             [SuccessCount, TotalOperations, NumClients]),
    io:format("Total concurrent time: ~p microseconds (~.2f milliseconds)~n", 
             [TotalElapsedTime, TotalElapsedTime/1000]),
    io:format("Average per operation: ~.2f microseconds (~.3f milliseconds)~n", 
             [AvgTimePerOp, AvgTimePerOp/1000]),
    io:format("Total throughput: ~.2f operations/second~n", [ThroughputOpsPerSec]),
    io:format("Throughput per client: ~.2f operations/second~n", [ThroughputOpsPerSec/NumClients]),
    
    %% Cleanup
    cleanup(),
    
    #{
        total_time_us => TotalElapsedTime,
        operations => TotalOperations,
        clients => NumClients,
        operations_per_client => OperationsPerClient,
        success_count => SuccessCount,
        avg_time_per_op_us => AvgTimePerOp,
        throughput_ops_per_sec => ThroughputOpsPerSec,
        throughput_per_client => ThroughputOpsPerSec / NumClients
    }.

%% Worker function for each concurrent instrumented client
concurrent_instrumented_client_worker(ClientId, NumOperations, Coordinator) ->
    %% Create unique client name
    ClientName = list_to_atom("test_client_" ++ integer_to_list(ClientId)),
    
    %% Setup instrumented client
    {ok, _} = client:start_link(ClientName),
    ok = gen_server:call(ClientName, connect),
    
    %% Wait for start signal
    receive
        start -> ok
    end,
    
    %% Execute operations
    Results = lists:map(fun(N) ->
        gen_server:call(ClientName, {send_number, N rem 50}, 10000)
    end, lists:seq(1, NumOperations)),
    
    %% Cleanup client
    gen_server:call(ClientName, disconnect),
    gen_server:stop(ClientName, normal, 1000),
    
    %% Send results back to coordinator
    Coordinator ! {multi_client_result, ClientId, Results}.

%% Collect results from all multi-client processes
collect_multi_client_results(0, Acc) -> Acc;
collect_multi_client_results(NumRemaining, Acc) ->
    receive
        {multi_client_result, ClientId, Results} ->
            collect_multi_client_results(NumRemaining - 1, [{ClientId, Results} | Acc])
    after 30000 -> % 30 second timeout
        error({timeout_waiting_for_multi_clients, NumRemaining})
    end.

%% Multi-client concurrent averaged test with multiple iterations
test_multi_client_averaged() ->
    test_multi_client_averaged(8, 2000). % Default: 4 clients, 25k ops each

test_multi_client_averaged(NumClients, OperationsPerClient) ->
    io:format("Multi-client instrumented averaged test (~p clients, ~p ops each, 20 iterations):~n", 
              [NumClients, OperationsPerClient]),
    
    NumIterations = 20,
    WarmupIterations = 5,
    
    %% Warmup runs (discarded)
    io:format("Running ~p warmup iterations...~n", [WarmupIterations]),
    lists:foreach(fun(_) ->
        run_multi_client_test(NumClients, OperationsPerClient),
        timer:sleep(200)  % Pause between runs
    end, lists:seq(1, WarmupIterations)),
    
    %% Actual measurement runs
    io:format("Running ~p measurement iterations...~n", [NumIterations]),
    Results = lists:map(fun(I) ->
        if I rem 5 == 0 -> 
            io:format("  Completed ~p/~p...~n", [I, NumIterations]); 
           true -> ok 
        end,
        Result = run_multi_client_test(NumClients, OperationsPerClient),
        timer:sleep(200),  % Pause between runs
        Result
    end, lists:seq(1, NumIterations)),
    
    %% Calculate averages
    TotalTimes = [maps:get(total_time_us, R) || R <- Results],
    AvgTimesPerOp = [maps:get(avg_time_per_op_us, R) || R <- Results],
    Throughputs = [maps:get(throughput_ops_per_sec, R) || R <- Results],
    ThroughputsPerClient = [maps:get(throughput_per_client, R) || R <- Results],
    
    AvgTotalTime = lists:sum(TotalTimes) / length(TotalTimes),
    AvgTimePerOp = lists:sum(AvgTimesPerOp) / length(AvgTimesPerOp),
    AvgThroughput = lists:sum(Throughputs) / length(Throughputs),
    AvgThroughputPerClient = lists:sum(ThroughputsPerClient) / length(ThroughputsPerClient),
    
    %% Calculate standard deviations
    StdDevTotalTime = std_dev(TotalTimes, AvgTotalTime),
    StdDevTimePerOp = std_dev(AvgTimesPerOp, AvgTimePerOp),
    StdDevThroughput = std_dev(Throughputs, AvgThroughput),
    StdDevThroughputPerClient = std_dev(ThroughputsPerClient, AvgThroughputPerClient),
    
    TotalOps = NumClients * OperationsPerClient,
    
    %% Display results
    io:format("~n=== MULTI-CLIENT INSTRUMENTED AVERAGED RESULTS (n=~p, ~p clients, ~p ops each) ===~n", 
              [NumIterations, NumClients, OperationsPerClient]),
    io:format("Total Operations per Run: ~p~n", [TotalOps]),
    io:format("Total Time:           ~.2f ± ~.2f ms~n", [AvgTotalTime/1000, StdDevTotalTime/1000]),
    io:format("Time per Operation:   ~.2f ± ~.2f μs~n", [AvgTimePerOp, StdDevTimePerOp]),
    io:format("Total Throughput:     ~.2f ± ~.2f ops/sec~n", [AvgThroughput, StdDevThroughput]),
    io:format("Throughput per Client: ~.2f ± ~.2f ops/sec~n", [AvgThroughputPerClient, StdDevThroughputPerClient]),
    
    %% Calculate concurrency benefit
    io:format("~n=== CONCURRENCY ANALYSIS ===~n"),
    EstimatedSequentialTime = AvgTimePerOp * TotalOps,
    SpeedupFactor = EstimatedSequentialTime / AvgTotalTime,
    EfficiencyPercent = (SpeedupFactor / NumClients) * 100,
    io:format("Estimated Sequential Time: ~.2f ms~n", [EstimatedSequentialTime/1000]),
    io:format("Concurrency Speedup:      ~.2fx~n", [SpeedupFactor]),
    io:format("Parallel Efficiency:      ~.1f%~n", [EfficiencyPercent]),
    
    %% Return comprehensive results
    #{
        iterations => NumIterations,
        clients => NumClients,
        operations_per_client => OperationsPerClient,
        total_operations_per_run => TotalOps,
        avg_total_time_us => AvgTotalTime,
        stddev_total_time_us => StdDevTotalTime,
        avg_time_per_op_us => AvgTimePerOp,
        stddev_time_per_op_us => StdDevTimePerOp,
        avg_throughput_ops_per_sec => AvgThroughput,
        stddev_throughput_ops_per_sec => StdDevThroughput,
        avg_throughput_per_client => AvgThroughputPerClient,
        stddev_throughput_per_client => StdDevThroughputPerClient,
        speedup_factor => SpeedupFactor,
        parallel_efficiency_percent => EfficiencyPercent,
        raw_results => Results
    }.

%% Quick multi-client test for manual execution
test_multi_client_quick() ->
    io:format("Quick multi-client instrumented test (2 clients):~n"),
    Result = run_multi_client_test(2, 1000),
    io:format("~n=== RESULT: ~.2f ms total, ~.2f ops/sec total throughput ===~n", 
             [maps:get(total_time_us, Result)/1000, maps:get(throughput_ops_per_sec, Result)]).