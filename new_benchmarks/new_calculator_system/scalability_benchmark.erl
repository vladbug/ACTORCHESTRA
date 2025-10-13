-module(scalability_benchmark).
-export([
    run_operations_scaling_test/0,
    run_clients_scaling_test/0,
    run_custom_scaling_test/2,
    run_comprehensive_scaling_test/0,
    % New matrix scaling functions
    run_matrix_scaling_test/0,
    run_matrix_scaling_test/2,
    export_results_to_csv/2
]).

-record(benchmark_config, {
    num_iterations = 10,
    num_clients = 5,
    operations_per_client = 10,
    warmup_iterations = 2
}).

-record(scaling_result, {
    test_name,
    system_type,           % baseline | instrumented
    test_type,            % batch | concurrent
    num_clients,
    operations_per_client,
    total_operations,
    avg_latency_us,
    stddev_latency_us,
    avg_total_time_us,
    stddev_total_time_us,
    avg_throughput_ops_per_sec,
    stddev_throughput_ops_per_sec,
    success_rate_pct,
    overhead_vs_baseline_pct
}).

%% ============================================================================
%% SCALING TEST CONFIGURATIONS
%% ============================================================================

%% Test how performance scales with number of operations (1 client, varying ops)
run_operations_scaling_test() ->
    io:format("=== OPERATIONS SCALING TEST ===~n"),
    io:format("Testing 1 client with varying number of operations~n"),
    
    %% Test configurations: 1 client, varying operations
    OpCounts = [1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000],
    
    TestConfigs = lists:map(fun(OpCount) ->
        {io_lib:format("ops_~p", [OpCount]), 
         #benchmark_config{
             num_iterations = 3,
             num_clients = 1,
             operations_per_client = OpCount,
             warmup_iterations = 5
         }}
    end, OpCounts),
    
    run_scaling_tests(TestConfigs, "operations_scaling_results.csv").

%% Test how performance scales with number of clients (fixed ops per client)
run_clients_scaling_test() ->
    io:format("=== CLIENTS SCALING TEST ===~n"),
    io:format("Testing varying clients with fixed operations per client~n"),
    
    %% Test configurations: varying clients, 100 ops per client
    ClientCounts = [1, 2, 4, 8, 16, 32, 64],
    
    TestConfigs = lists:map(fun(ClientCount) ->
        {io_lib:format("clients_~p", [ClientCount]), 
         #benchmark_config{
             num_iterations = 20,
             num_clients = ClientCount,
             operations_per_client = 2000,
             warmup_iterations = 5
         }}
    end, ClientCounts),
    
    run_scaling_tests(TestConfigs, "clients_scaling_results.csv").

%% Custom scaling test with user-defined parameters
run_custom_scaling_test(ParameterName, ConfigList) ->
    io:format("=== CUSTOM SCALING TEST: ~s ===~n", [ParameterName]),
    
    TestConfigs = lists:map(fun({Label, Config}) ->
        {Label, Config}
    end, ConfigList),
    
    Filename = io_lib:format("~s_scaling_results.csv", [ParameterName]),
    run_scaling_tests(TestConfigs, lists:flatten(Filename)).

%% Comprehensive test covering multiple scaling dimensions
run_comprehensive_scaling_test() ->
    io:format("=== COMPREHENSIVE SCALING TEST ===~n"),
    io:format("This will take 30-45 minutes...~n"),
    
    %% Multiple test scenarios
    TestScenarios = [
        %% Small scale tests
        {"small_1c_100ops", #benchmark_config{num_clients=1, operations_per_client=100, num_iterations=5}},
        {"small_1c_500ops", #benchmark_config{num_clients=1, operations_per_client=500, num_iterations=5}},
        {"small_5c_100ops", #benchmark_config{num_clients=5, operations_per_client=100, num_iterations=5}},
        
        %% Medium scale tests
        {"medium_1c_1000ops", #benchmark_config{num_clients=1, operations_per_client=1000, num_iterations=4}},
        {"medium_10c_100ops", #benchmark_config{num_clients=10, operations_per_client=100, num_iterations=4}},
        {"medium_5c_500ops", #benchmark_config{num_clients=5, operations_per_client=500, num_iterations=4}},
        
        %% Large scale tests
        {"large_1c_5000ops", #benchmark_config{num_clients=1, operations_per_client=5000, num_iterations=3}},
        {"large_20c_100ops", #benchmark_config{num_clients=20, operations_per_client=100, num_iterations=3}},
        {"large_10c_1000ops", #benchmark_config{num_clients=10, operations_per_client=1000, num_iterations=3}}
    ],
    
    run_scaling_tests(TestScenarios, "comprehensive_scaling_results.csv").

%% ============================================================================
%% CORE SCALING TEST EXECUTION
%% ============================================================================

run_scaling_tests(TestConfigs, OutputFilename) ->
    io:format("Running ~p test configurations...~n", [length(TestConfigs)]),
    
    %% Run all test configurations and collect results
    AllResults = lists:foldl(fun({TestName, Config}, AccResults) ->
        io:format("~nRunning test: ~s~n", [TestName]),
        io:format("  Config: ~p clients, ~p ops/client, ~p iterations~n", 
                  [Config#benchmark_config.num_clients,
                   Config#benchmark_config.operations_per_client,
                   Config#benchmark_config.num_iterations]),
        
        %% Show what type of execution will be performed
        case Config#benchmark_config.num_clients of
            1 -> 
                io:format("  Note: Single client - sequential execution~n");
            N when N > 1 ->
                io:format("  Note: ~p clients - concurrent execution (spawned processes)~n", [N])
        end,
        
        %% Run baseline system
        io:format("  Running baseline...~n"),
        BaselineResults = run_single_scaling_test(baseline, Config),
        
        %% Small delay between systems
        timer:sleep(500),
        
        %% Run instrumented system  
        io:format("  Running instrumented...~n"),
        InstrumentedResults = run_single_scaling_test(instrumented, Config),
        
        %% Convert to scaling result records
        BaselineScalingResults = convert_to_scaling_results(TestName, BaselineResults, baseline, undefined),
        InstrumentedScalingResults = convert_to_scaling_results(TestName, InstrumentedResults, instrumented, BaselineResults),
        
        %% Show simple completion message instead of detailed comparison
        io:format("  -> Test completed successfully~n"),
        
        AccResults ++ BaselineScalingResults ++ InstrumentedScalingResults
    end, [], TestConfigs),
    
    %% Export to CSV
    export_results_to_csv(AllResults, OutputFilename),
    
    %% Show summary
    show_scaling_summary(AllResults),
    
    AllResults.

run_single_scaling_test(SystemType, Config) ->
    case SystemType of
        baseline ->
            run_baseline_scaling_test(Config);
        instrumented ->
            run_instrumented_scaling_test(Config)
    end.

run_baseline_scaling_test(Config) ->
    %% Single unified test that adapts based on client count
    Result = run_baseline_unified_test(Config),
    #{unified => Result}.

run_instrumented_scaling_test(Config) ->
    %% Single unified test that adapts based on client count
    Result = run_instrumented_unified_test(Config),
    #{unified => Result}.

%% ============================================================================
%% INDIVIDUAL TEST IMPLEMENTATIONS
%% ============================================================================

run_baseline_unified_test(Config) ->
    NumIterations = Config#benchmark_config.num_iterations,
    WarmupIterations = Config#benchmark_config.warmup_iterations,
    NumClients = Config#benchmark_config.num_clients,
    OpsPerClient = Config#benchmark_config.operations_per_client,
    
    %% Warmup runs
    lists:foreach(fun(_) ->
        run_single_baseline_unified_iteration(NumClients, OpsPerClient),
        timer:sleep(100)
    end, lists:seq(1, WarmupIterations)),
    
    %% Actual measurement runs
    Results = lists:map(fun(_) ->
        Result = run_single_baseline_unified_iteration(NumClients, OpsPerClient),
        timer:sleep(100),
        Result
    end, lists:seq(1, NumIterations)),
    
    aggregate_results(Results, unified).

run_single_baseline_unified_iteration(1, OpsPerClient) ->
    %% Single client - sequential execution
    {ok, _} = baseline_slow_central_server:start_link(),
    {ok, _} = baseline_client:start_link(scale_client),
    ok = gen_server:call(scale_client, connect),
    
    StartTime = erlang:system_time(microsecond),
    
    Results = lists:map(fun(N) ->
        OpStart = erlang:system_time(microsecond),
        Result = gen_server:call(scale_client, {send_number, N rem 50}, 10000),
        OpEnd = erlang:system_time(microsecond),
        {Result, OpEnd - OpStart}
    end, lists:seq(1, OpsPerClient)),
    
    EndTime = erlang:system_time(microsecond),
    
    gen_server:call(scale_client, disconnect),
    cleanup_baseline(),
    
    extract_iteration_metrics(Results, StartTime, EndTime);

run_single_baseline_unified_iteration(NumClients, OpsPerClient) when NumClients > 1 ->
    %% Multiple clients - spawn a process for each client
    {ok, _} = baseline_slow_central_server:start_link(),
    
    ClientNames = lists:map(fun(N) ->
        ClientName = list_to_atom("scale_client_" ++ integer_to_list(N)),
        {ok, _} = baseline_client:start_link(ClientName),
        ok = gen_server:call(ClientName, connect),
        ClientName
    end, lists:seq(1, NumClients)),
    
    StartTime = erlang:system_time(microsecond),
    ParentPid = self(),
    
    %% Spawn one process per client
    lists:foreach(fun({ClientName, ClientId}) ->
        spawn(fun() ->
            ClientResults = lists:map(fun(OpId) ->
                OpStart = erlang:system_time(microsecond),
                Result = gen_server:call(ClientName, {send_number, (ClientId * 100 + OpId) rem 50}, 10000),
                OpEnd = erlang:system_time(microsecond),
                {Result, OpEnd - OpStart}
            end, lists:seq(1, OpsPerClient)),
            ParentPid ! {client_done, ClientId, ClientResults}
        end)
    end, lists:zip(ClientNames, lists:seq(1, NumClients))),
    
    %% Collect results from all clients
    AllResults = collect_client_results(NumClients, []),
    
    EndTime = erlang:system_time(microsecond),
    
    lists:foreach(fun(ClientName) ->
        gen_server:call(ClientName, disconnect)
    end, ClientNames),
    cleanup_baseline(),
    
    %% Flatten results from all clients
    FlatResults = lists:flatten([ClientResults || {_ClientId, ClientResults} <- AllResults]),
    extract_iteration_metrics(FlatResults, StartTime, EndTime).

run_instrumented_unified_test(Config) ->
    NumIterations = Config#benchmark_config.num_iterations,
    WarmupIterations = Config#benchmark_config.warmup_iterations,
    NumClients = Config#benchmark_config.num_clients,
    OpsPerClient = Config#benchmark_config.operations_per_client,
    
    %% Warmup runs
    lists:foreach(fun(_) ->
        run_single_instrumented_unified_iteration(NumClients, OpsPerClient),
        timer:sleep(100)
    end, lists:seq(1, WarmupIterations)),
    
    %% Actual measurement runs
    Results = lists:map(fun(_) ->
        Result = run_single_instrumented_unified_iteration(NumClients, OpsPerClient),
        timer:sleep(100),
        Result
    end, lists:seq(1, NumIterations)),
    
    aggregate_results(Results, unified).

run_single_instrumented_unified_iteration(1, OpsPerClient) ->
    %% Single client - sequential execution
    {ok, _} = monitor:start(),
    {ok, _} = simple_wrapper:start_link(),
    {ok, _} = slow_central_server:start_link(),
    {ok, _} = client:start_link(scale_client),
    ok = gen_server:call(scale_client, connect),
    
    StartTime = erlang:system_time(microsecond),
    
    Results = lists:map(fun(N) ->
        OpStart = erlang:system_time(microsecond),
        Result = gen_server:call(scale_client, {send_number, N rem 50}, 10000),
        OpEnd = erlang:system_time(microsecond),
        {Result, OpEnd - OpStart}
    end, lists:seq(1, OpsPerClient)),
    
    EndTime = erlang:system_time(microsecond),
    
    gen_server:call(scale_client, disconnect),
    cleanup_instrumented(),
    
    extract_iteration_metrics(Results, StartTime, EndTime);

run_single_instrumented_unified_iteration(NumClients, OpsPerClient) when NumClients > 1 ->
    %% Multiple clients - spawn a process for each client
    {ok, _} = monitor:start(),
    {ok, _} = simple_wrapper:start_link(),
    {ok, _} = slow_central_server:start_link(),
    
    ClientNames = lists:map(fun(N) ->
        ClientName = list_to_atom("scale_client_" ++ integer_to_list(N)),
        {ok, _} = client:start_link(ClientName),
        ok = gen_server:call(ClientName, connect),
        ClientName
    end, lists:seq(1, NumClients)),
    
    StartTime = erlang:system_time(microsecond),
    ParentPid = self(),
    
    %% Spawn one process per client
    lists:foreach(fun({ClientName, ClientId}) ->
        spawn(fun() ->
            ClientResults = lists:map(fun(OpId) ->
                OpStart = erlang:system_time(microsecond),
                Result = gen_server:call(ClientName, {send_number, (ClientId * 100 + OpId) rem 50}, 10000),
                OpEnd = erlang:system_time(microsecond),
                {Result, OpEnd - OpStart}
            end, lists:seq(1, OpsPerClient)),
            ParentPid ! {client_done, ClientId, ClientResults}
        end)
    end, lists:zip(ClientNames, lists:seq(1, NumClients))),
    
    %% Collect results from all clients
    AllResults = collect_client_results(NumClients, []),
    
    EndTime = erlang:system_time(microsecond),
    
    lists:foreach(fun(ClientName) ->
        gen_server:call(ClientName, disconnect)
    end, ClientNames),
    cleanup_instrumented(),
    
    %% Flatten results from all clients
    FlatResults = lists:flatten([ClientResults || {_ClientId, ClientResults} <- AllResults]),
    extract_iteration_metrics(FlatResults, StartTime, EndTime).

%% ============================================================================
%% RESULT PROCESSING AND ANALYSIS
%% ============================================================================

extract_iteration_metrics(Results, StartTime, EndTime) ->
    TotalTime = EndTime - StartTime,
    {Successes, Latencies} = lists:foldl(fun({Result, Latency}, {SAcc, LAcc}) ->
        case Result of
            {ok, _} -> {SAcc + 1, [Latency | LAcc]};
            _ -> {SAcc, LAcc}
        end
    end, {0, []}, Results),
    
    #{
        total_time_us => TotalTime,
        operations => length(Results),
        success_count => Successes,
        latencies => Latencies,
        throughput_ops_per_sec => Successes / (TotalTime / 1000000)
    }.

aggregate_results(IterationResults, TestType) ->
    TotalTimes = [maps:get(total_time_us, R) || R <- IterationResults],
    AllLatencies = lists:flatten([maps:get(latencies, R) || R <- IterationResults]),
    Throughputs = [maps:get(throughput_ops_per_sec, R) || R <- IterationResults],
    TotalOps = lists:sum([maps:get(operations, R) || R <- IterationResults]),
    TotalSuccesses = lists:sum([maps:get(success_count, R) || R <- IterationResults]),
    
    #{
        test_type => TestType,
        avg_total_time_us => avg(TotalTimes),
        stddev_total_time_us => stddev(TotalTimes),
        avg_latency_us => avg(AllLatencies),
        stddev_latency_us => stddev(AllLatencies),
        avg_throughput_ops_per_sec => avg(Throughputs),
        stddev_throughput_ops_per_sec => stddev(Throughputs),
        total_operations => TotalOps,
        total_successes => TotalSuccesses,
        success_rate_pct => (TotalSuccesses / TotalOps) * 100
    }.

convert_to_scaling_results(TestName, SystemResults, SystemType, BaselineResults) ->
    UnifiedResult = maps:get(unified, SystemResults),
    
    %% Calculate overhead if we have baseline results
    Overhead = case BaselineResults of
        undefined -> undefined;
        _ -> 
            BaselineUnified = maps:get(unified, BaselineResults),
            calculate_overhead_pct(maps:get(avg_latency_us, BaselineUnified), maps:get(avg_latency_us, UnifiedResult))
    end,
    
    %% Extract client and operation counts from test name and results
    {NumClients, OpsPerClient} = extract_test_params(TestName, UnifiedResult, UnifiedResult),
    
    UnifiedScalingResult = #scaling_result{
        test_name = TestName,
        system_type = SystemType,
        test_type = unified,
        num_clients = NumClients,
        operations_per_client = OpsPerClient,
        total_operations = maps:get(total_operations, UnifiedResult),
        avg_latency_us = maps:get(avg_latency_us, UnifiedResult),
        stddev_latency_us = maps:get(stddev_latency_us, UnifiedResult),
        avg_total_time_us = maps:get(avg_total_time_us, UnifiedResult),
        stddev_total_time_us = maps:get(stddev_total_time_us, UnifiedResult),
        avg_throughput_ops_per_sec = maps:get(avg_throughput_ops_per_sec, UnifiedResult),
        stddev_throughput_ops_per_sec = maps:get(stddev_throughput_ops_per_sec, UnifiedResult),
        success_rate_pct = maps:get(success_rate_pct, UnifiedResult),
        overhead_vs_baseline_pct = Overhead
    },
    
    [UnifiedScalingResult].

extract_test_params(TestName, UnifiedResult, _) ->
    %% Try matrix format first: "matrix_4c_1000ops"
    case re:run(TestName, "matrix_([0-9]+)c_([0-9]+)ops", [{capture, all_but_first, list}]) of
        {match, [ClientsStr, OpsStr]} ->
            {list_to_integer(ClientsStr), list_to_integer(OpsStr)};
        nomatch ->
            %% Try legacy ops format: "ops_1000"  
            case re:run(TestName, "ops_([0-9]+)", [{capture, all_but_first, list}]) of
                {match, [OpsStr]} ->
                    {1, list_to_integer(OpsStr)};
                nomatch ->
                    %% Try legacy clients format: "clients_4"
                    case re:run(TestName, "clients_([0-9]+)", [{capture, all_but_first, list}]) of
                        {match, [ClientsStr]} ->
                            {list_to_integer(ClientsStr), 100};
                        nomatch ->
                            %% Fall back to inferring from results
                            TotalOps = maps:get(total_operations, UnifiedResult),
                            {1, TotalOps}
                    end
            end
    end.

%% ============================================================================
%% CSV EXPORT
%% ============================================================================

export_results_to_csv(Results, Filename) ->
    Header = "test_name,system_type,test_type,num_clients,operations_per_client,total_operations,"
             "avg_latency_us,stddev_latency_us,avg_total_time_us,stddev_total_time_us,"
             "avg_throughput_ops_per_sec,stddev_throughput_ops_per_sec,success_rate_pct,overhead_vs_baseline_pct\n",
    
    Rows = lists:map(fun(Result) ->
        OverheadStr = case Result#scaling_result.overhead_vs_baseline_pct of
            undefined -> "";
            Value -> io_lib:format("~.2f", [Value])
        end,
        
        io_lib:format("~s,~p,~p,~p,~p,~p,~.2f,~.2f,~.2f,~.2f,~.2f,~.2f,~.2f,~s~n", [
            Result#scaling_result.test_name,
            Result#scaling_result.system_type,
            Result#scaling_result.test_type,
            Result#scaling_result.num_clients,
            Result#scaling_result.operations_per_client,
            Result#scaling_result.total_operations,
            Result#scaling_result.avg_latency_us,
            Result#scaling_result.stddev_latency_us,
            Result#scaling_result.avg_total_time_us,
            Result#scaling_result.stddev_total_time_us,
            Result#scaling_result.avg_throughput_ops_per_sec,
            Result#scaling_result.stddev_throughput_ops_per_sec,
            Result#scaling_result.success_rate_pct,
            OverheadStr
        ])
    end, Results),
    
    file:write_file(Filename, [Header | Rows]),
    io:format("Results exported to: ~s~n", [Filename]).

%% ============================================================================
%% DISPLAY FUNCTIONS
%% ============================================================================

show_quick_comparison(TestName, BaselineResults, InstrumentedResults) ->
    BatchBaseline = maps:get(avg_latency_us, maps:get(batch, BaselineResults)),
    BatchInstrumented = maps:get(avg_latency_us, maps:get(batch, InstrumentedResults)),
    BatchOverhead = calculate_overhead_pct(BatchBaseline, BatchInstrumented),
    
    %% Format the message parts separately to avoid encoding issues
    OverheadStr = lists:flatten(io_lib:format("~.1f", [BatchOverhead])),
    BaselineStr = lists:flatten(io_lib:format("~.0f", [BatchBaseline])),
    InstrumentedStr = lists:flatten(io_lib:format("~.0f", [BatchInstrumented])),
    
    io:format("  -> Batch latency overhead: ~s% (~s us -> ~s us)~n", 
              [OverheadStr, BaselineStr, InstrumentedStr]).

show_scaling_summary(AllResults) ->
    io:format("~n=== SCALING TEST SUMMARY ===~n"),
    
    %% Group results by system type
    InstrumentedResults = [R || R <- AllResults, R#scaling_result.system_type == instrumented],
    
    io:format("Overhead Summary:~n"),
    lists:foreach(fun(Result) ->
        case Result#scaling_result.overhead_vs_baseline_pct of
            undefined -> 
                io:format("  ~p: No baseline comparison available~n", [Result#scaling_result.test_name]);
            Overhead when is_number(Overhead) ->
                %% Simple formatting without complex conversions
                TestName = Result#scaling_result.test_name,
                InstrumentedLatency = Result#scaling_result.avg_latency_us,
                ClientInfo = case Result#scaling_result.num_clients of
                    1 -> "sequential";
                    N -> io_lib:format("~p clients", [N])
                end,
                io:format("  ~p (~s): overhead ~p%, latency ~p us~n", 
                         [TestName, ClientInfo, round(Overhead * 10) / 10, round(InstrumentedLatency)])
        end
    end, InstrumentedResults).

find_baseline_latency(InstrumentedResult, BaselineResults) ->
    case lists:keyfind(InstrumentedResult#scaling_result.test_name, #scaling_result.test_name, BaselineResults) of
        false -> 0.0;
        BaselineResult -> BaselineResult#scaling_result.avg_latency_us
    end.

%% ============================================================================
%% HELPER FUNCTIONS
%% ============================================================================

collect_client_results(0, Acc) ->
    Acc;
collect_client_results(Remaining, Acc) ->
    receive
        {client_done, ClientId, Results} ->
            collect_client_results(Remaining - 1, [{ClientId, Results} | Acc])
    after 30000 ->
        io:format("TIMEOUT: Still waiting for ~p client results~n", [Remaining]),
        Acc
    end.

cleanup_baseline() ->
    Processes = [scale_client, baseline_slow_central_server, baseline_slow_proc_add10, baseline_slow_proc_mul2],
    lists:foreach(fun(Name) -> safe_stop_process(Name) end, Processes),
    cleanup_numbered_clients(),
    timer:sleep(50).

cleanup_instrumented() ->
    Processes = [scale_client, slow_central_server, slow_proc_add10, slow_proc_mul2, simple_wrapper],
    lists:foreach(fun(Name) -> safe_stop_process(Name) end, Processes),
    cleanup_numbered_clients(),
    catch monitor:stop(),
    timer:sleep(50).

cleanup_numbered_clients() ->
    lists:foreach(fun(N) ->
        ClientName = list_to_atom("scale_client_" ++ integer_to_list(N)),
        safe_stop_process(ClientName)
    end, lists:seq(1, 1000)).

safe_stop_process(Name) ->
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
    end.

avg([]) -> 0.0;
avg(List) -> lists:sum(List) / length(List).

stddev([]) -> 0.0;
stddev([_]) -> 0.0;
stddev(List) ->
    Mean = avg(List),
    Variance = lists:sum([math:pow(X - Mean, 2) || X <- List]) / (length(List) - 1),
    math:sqrt(Variance).

calculate_overhead_pct(Baseline, Measured) ->
    ((Measured - Baseline) / Baseline) * 100.


%% Test performance matrix: varying both clients and operations per client
run_matrix_scaling_test() ->
    run_matrix_scaling_test([1,2,4,8,16,32,64,128,256,512], [2000]).

run_matrix_scaling_test(ClientCounts, OperationCounts) ->
    io:format("=== MATRIX SCALING TEST ===~n"),
    io:format("Testing ~p client counts × ~p operation counts = ~p combinations~n", 
              [length(ClientCounts), length(OperationCounts), 
               length(ClientCounts) * length(OperationCounts)]),
    io:format("Client counts: ~p~n", [ClientCounts]),
    io:format("Operations per client: ~p~n", [OperationCounts]),
    io:format("This will take ~p-~p minutes...~n", 
              [length(ClientCounts) * length(OperationCounts) * 2 div 60,
               length(ClientCounts) * length(OperationCounts) * 3 div 60]),
    
    %% Generate all combinations
    TestConfigs = lists:flatten([
        lists:map(fun(OpCount) ->
            TestName = io_lib:format("matrix_~pc_~pops", [ClientCount, OpCount]),
            {lists:flatten(TestName), 
             #benchmark_config{
                 num_iterations = determine_iterations(ClientCount, OpCount),
                 num_clients = ClientCount,
                 operations_per_client = OpCount,
                 warmup_iterations = determine_warmup_iterations(ClientCount, OpCount)
             }}
        end, OperationCounts)
        || ClientCount <- ClientCounts
    ]),
    
    io:format("Generated ~p test configurations~n", [length(TestConfigs)]),
    
    %% Run the matrix tests
    Results = run_scaling_tests(TestConfigs, "matrix_scaling_results.csv"),
    
    %% Generate additional analysis
    generate_matrix_analysis(Results, ClientCounts, OperationCounts),
    
    Results.

%% Determine number of iterations based on test size (fewer iterations for larger tests)
determine_iterations(ClientCount, OpCount) ->
    TotalOps = ClientCount * OpCount,
    if 
        TotalOps =< 1000 -> 15;
        TotalOps =< 5000 -> 10;
        TotalOps =< 20000 -> 8;
        TotalOps =< 50000 -> 5;
        true -> 3
    end.

determine_warmup_iterations(ClientCount, OpCount) ->
    TotalOps = ClientCount * OpCount,
    if 
        TotalOps =< 5000 -> 3;
        TotalOps =< 20000 -> 2;
        true -> 1
    end.

%% Generate additional matrix-specific analysis
generate_matrix_analysis(Results, ClientCounts, OperationCounts) ->
    io:format("~n=== MATRIX ANALYSIS ===~n"),
    
    %% Export separate files for easier analysis
    export_matrix_pivot_tables(Results, ClientCounts, OperationCounts).

export_matrix_pivot_tables(Results, ClientCounts, OperationCounts) ->
    %% Export baseline latency matrix
    export_latency_matrix(Results, baseline, ClientCounts, OperationCounts, "matrix_baseline_latency.csv"),
    
    %% Export instrumented latency matrix  
    export_latency_matrix(Results, instrumented, ClientCounts, OperationCounts, "matrix_instrumented_latency.csv"),
    
    %% Export overhead matrix
    export_overhead_matrix(Results, ClientCounts, OperationCounts, "matrix_overhead.csv"),
    
    %% Export throughput matrices
    export_throughput_matrix(Results, baseline, ClientCounts, OperationCounts, "matrix_baseline_throughput.csv"),
    export_throughput_matrix(Results, instrumented, ClientCounts, OperationCounts, "matrix_instrumented_throughput.csv").

export_latency_matrix(Results, SystemType, ClientCounts, OperationCounts, Filename) ->
    %% Create CSV with clients as rows, operations as columns
    Header = "clients," ++ string:join([integer_to_list(Op) ++ "_ops" || Op <- OperationCounts], ",") ++ "\n",
    
    Rows = lists:map(fun(ClientCount) ->
        Row = integer_to_list(ClientCount) ++ "," ++
              string:join([
                  case find_result_latency(Results, SystemType, ClientCount, OpCount) of
                      undefined -> "N/A";
                      Latency -> io_lib:format("~.2f", [Latency])
                  end || OpCount <- OperationCounts
              ], ",") ++ "\n",
        lists:flatten(Row)
    end, ClientCounts),
    
    file:write_file(Filename, [Header | Rows]),
    io:format("Exported ~p latency matrix: ~s~n", [SystemType, Filename]).

export_throughput_matrix(Results, SystemType, ClientCounts, OperationCounts, Filename) ->
    %% Create CSV with clients as rows, operations as columns
    Header = "clients," ++ string:join([integer_to_list(Op) ++ "_ops" || Op <- OperationCounts], ",") ++ "\n",
    
    Rows = lists:map(fun(ClientCount) ->
        Row = integer_to_list(ClientCount) ++ "," ++
              string:join([
                  case find_result_throughput(Results, SystemType, ClientCount, OpCount) of
                      undefined -> "N/A";
                      Throughput -> io_lib:format("~.2f", [Throughput])
                  end || OpCount <- OperationCounts
              ], ",") ++ "\n",
        lists:flatten(Row)
    end, ClientCounts),
    
    file:write_file(Filename, [Header | Rows]),
    io:format("Exported ~p throughput matrix: ~s~n", [SystemType, Filename]).

export_overhead_matrix(Results, ClientCounts, OperationCounts, Filename) ->
    %% Create CSV with clients as rows, operations as columns
    Header = "clients," ++ string:join([integer_to_list(Op) ++ "_ops" || Op <- OperationCounts], ",") ++ "\n",
    
    Rows = lists:map(fun(ClientCount) ->
        Row = integer_to_list(ClientCount) ++ "," ++
              string:join([
                  case find_result_overhead(Results, ClientCount, OpCount) of
                      undefined -> "N/A";
                      Overhead -> io_lib:format("~.2f", [Overhead])
                  end || OpCount <- OperationCounts
              ], ",") ++ "\n",
        lists:flatten(Row)
    end, ClientCounts),
    
    file:write_file(Filename, [Header | Rows]),
    io:format("Exported overhead matrix: ~s~n", [Filename]).

%% Helper functions to find specific results in the result set
find_result_latency(Results, SystemType, ClientCount, OpCount) ->
    case find_matching_result(Results, SystemType, ClientCount, OpCount) of
        undefined -> undefined;
        Result -> Result#scaling_result.avg_latency_us
    end.

find_result_throughput(Results, SystemType, ClientCount, OpCount) ->
    case find_matching_result(Results, SystemType, ClientCount, OpCount) of
        undefined -> undefined;
        Result -> Result#scaling_result.avg_throughput_ops_per_sec
    end.

find_result_overhead(Results, ClientCount, OpCount) ->
    case find_matching_result(Results, instrumented, ClientCount, OpCount) of
        undefined -> undefined;
        Result -> Result#scaling_result.overhead_vs_baseline_pct
    end.

find_matching_result(Results, SystemType, ClientCount, OpCount) ->
    case [R || R <- Results,
               R#scaling_result.system_type == SystemType,
               R#scaling_result.num_clients == ClientCount,
               R#scaling_result.operations_per_client == OpCount] of
        [Result|_] -> Result;
        [] -> undefined
    end.