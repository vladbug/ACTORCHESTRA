-module(chat_benchmark).
-export([run_benchmark/0, cleanup/0]).

%% Clean benchmark that mirrors the manual tests exactly
run_benchmark() ->
    io:format("=== CHAT SYSTEM BENCHMARK ===~n"),
    io:format("Measuring end-to-end performance difference between baseline and instrumented systems~n~n"),
    
    StartTime = erlang:system_time(millisecond),
    
    %% Test configurations - same operations, different system sizes
    SessionConfigs = [
        {5, 30},
        {10, 30},
        {20, 30},
        {50, 20},
        {100, 10},
        {200, 10}
    ],
    Iterations = 10,  % Multiple iterations for statistical validity
    
    Results = lists:flatten(lists:map(fun({ClientCount, MessagesPerClient}) ->
        io:format("~n--- Testing ~p-client chat session (~p messages each, ~p iterations) ---~n", 
                 [ClientCount, MessagesPerClient, Iterations]),
        
        %% Run baseline session tests
        BaselineResults = run_baseline_iterations(ClientCount, MessagesPerClient, Iterations),
        
        %% Run instrumented session tests  
        InstrumentedResults = run_instrumented_iterations(ClientCount, MessagesPerClient, Iterations),
        
        %% Calculate and report results
        calculate_session_comparison(ClientCount, MessagesPerClient, BaselineResults, InstrumentedResults)
    end, SessionConfigs)),
    
    EndTime = erlang:system_time(millisecond),
    TotalTime = EndTime - StartTime,
    
    %% Generate final report
    generate_final_report(Results, TotalTime),
    save_results_to_csv(Results, TotalTime),
    
    Results.

%% Run baseline iterations with proper cleanup between each
run_baseline_iterations(ClientCount, MessagesPerClient, Iterations) ->
    io:format("  Running ~p baseline iterations...~n", [Iterations]),
    
    lists:map(fun(Iter) ->
        io:format("    Baseline iteration ~p/~p: ", [Iter, Iterations]),
        
        %% Full cleanup and stabilization
        complete_system_cleanup(),
        system_stabilization_wait(),
        
        %% Warmup - same pattern as main test but smaller
        run_baseline_warmup(min(ClientCount, 3), max(3, MessagesPerClient div 2)),
        complete_system_cleanup(),
        system_stabilization_wait(),
        
        %% Run the actual test
        {SessionTime, SessionResult} = timer:tc(fun() ->
            run_complete_baseline_session(ClientCount, MessagesPerClient)
        end),
        
        %% Cleanup after test
        complete_system_cleanup(),
        
        %% Calculate metrics
        Metrics = calculate_session_metrics("Baseline", SessionResult, SessionTime),
        io:format("~.1f ms total~n", [SessionTime/1000]),
        Metrics
    end, lists:seq(1, Iterations)).

%% Run instrumented iterations with proper cleanup between each
run_instrumented_iterations(ClientCount, MessagesPerClient, Iterations) ->
    io:format("  Running ~p instrumented iterations...~n", [Iterations]),
    
    lists:map(fun(Iter) ->
        io:format("    Instrumented iteration ~p/~p: ", [Iter, Iterations]),
        
        %% Full cleanup and stabilization
        complete_system_cleanup(),
        system_stabilization_wait(),
        
        %% Warmup - same pattern as main test but smaller
        run_instrumented_warmup(min(ClientCount, 3), max(3, MessagesPerClient div 2)),
        complete_system_cleanup(),
        system_stabilization_wait(),
        
        %% Run the actual test
        {SessionTime, SessionResult} = timer:tc(fun() ->
            run_complete_instrumented_session(ClientCount, MessagesPerClient)
        end),
        
        %% Cleanup after test
        complete_system_cleanup(),
        
        %% Calculate metrics
        Metrics = calculate_session_metrics("Instrumented", SessionResult, SessionTime),
        io:format("~.1f ms total~n", [SessionTime/1000]),
        Metrics
    end, lists:seq(1, Iterations)).

%% Complete baseline session test - mirrors manual_baseline_chat_test:run_session_test/0
run_complete_baseline_session(ClientCount, MessagesPerClient) ->
    RoomName = benchmark_room,
    
    %% Setup baseline system (exactly like manual test)
    start_process_safely(baseline_chat_server, fun() -> baseline_chat_server:start_link() end),
    ClientNames = generate_baseline_client_names(ClientCount),
    
    %% Start and register all clients
    lists:foreach(fun(Name) ->
        start_process_safely(Name, fun() -> baseline_client:start_link(Name) end),
        gen_server:call(Name, register)
    end, ClientNames),
    
    %% PHASE 1: All clients join the room
    JoinStart = erlang:system_time(microsecond),
    JoinResults = lists:map(fun(ClientName) ->
        try
            gen_server:call(ClientName, {join_room, RoomName}, 10000),
            success
        catch
            Error:Reason -> {error, Error, Reason}
        end
    end, ClientNames),
    JoinEnd = erlang:system_time(microsecond),
    JoinTime = JoinEnd - JoinStart,
    
    %% PHASE 2: All clients send messages concurrently
    ParentPid = self(),
    MessageStart = erlang:system_time(microsecond),
    
    lists:foreach(fun(ClientName) ->
        spawn(fun() ->
            run_client_messaging_session(baseline, ClientName, RoomName, MessagesPerClient, ParentPid)
        end)
    end, ClientNames),
    
    %% Collect all message results
    TotalMessages = ClientCount * MessagesPerClient,
    {MessageSuccesses, MessageFailures} = collect_message_results(TotalMessages, [], []),
    MessageEnd = erlang:system_time(microsecond),
    MessageTime = MessageEnd - MessageStart,
    
    %% PHASE 3: All clients leave the room
    LeaveStart = erlang:system_time(microsecond),
    LeaveResults = lists:map(fun(ClientName) ->
        try
            gen_server:call(ClientName, {leave_room, RoomName}, 10000),
            success
        catch
            Error:Reason -> {error, Error, Reason}
        end
    end, ClientNames),
    LeaveEnd = erlang:system_time(microsecond),
    LeaveTime = LeaveEnd - LeaveStart,
    
    %% Return session results
    #{
        join_time_us => JoinTime,
        message_time_us => MessageTime,
        leave_time_us => LeaveTime,
        total_session_time_us => JoinTime + MessageTime + LeaveTime,
        message_successes => length(MessageSuccesses),
        message_failures => length(MessageFailures),
        message_latencies => MessageSuccesses,  % Contains list of individual latencies
        join_successes => length([R || R <- JoinResults, R =:= success]),
        leave_successes => length([R || R <- LeaveResults, R =:= success]),
        clients => ClientCount,
        messages_per_client => MessagesPerClient,
        total_messages => TotalMessages
    }.

run_complete_instrumented_session(ClientCount, MessagesPerClient) ->
    RoomName = benchmark_room,
    
    %% Setup instrumented system (exactly like manual test)
    start_process_safely(monitor, fun() -> monitor:start() end),
    start_process_safely(simple_wrapper, fun() -> simple_wrapper:start_link() end),
    start_process_safely(chat_server, fun() -> chat_server:start_link() end),
    ClientNames = generate_instrumented_client_names(ClientCount),
    
    %% Start and register all clients
    lists:foreach(fun(Name) ->
        start_process_safely(Name, fun() -> client:start_link(Name) end),
        gen_server:call(Name, register)
    end, ClientNames),
    
    %% PHASE 1: All clients join the room (identical to baseline)
    JoinStart = erlang:system_time(microsecond),
    JoinResults = lists:map(fun(ClientName) ->
        try
            gen_server:call(ClientName, {join_room, RoomName}, 10000),
            success
        catch
            Error:Reason -> {error, Error, Reason}
        end
    end, ClientNames),
    JoinEnd = erlang:system_time(microsecond),
    JoinTime = JoinEnd - JoinStart,
    
    %% PHASE 2: All clients send messages concurrently (identical pattern)
    ParentPid = self(),
    MessageStart = erlang:system_time(microsecond),
    
    lists:foreach(fun(ClientName) ->
        spawn(fun() ->
            run_client_messaging_session(instrumented, ClientName, RoomName, MessagesPerClient, ParentPid)
        end)
    end, ClientNames),
    
    %% Collect all message results (identical to baseline)
    TotalMessages = ClientCount * MessagesPerClient,
    {MessageSuccesses, MessageFailures} = collect_message_results(TotalMessages, [], []),
    MessageEnd = erlang:system_time(microsecond),
    MessageTime = MessageEnd - MessageStart,
    
    %% PHASE 3: All clients leave the room (identical to baseline)
    LeaveStart = erlang:system_time(microsecond),
    LeaveResults = lists:map(fun(ClientName) ->
        try
            gen_server:call(ClientName, {leave_room, RoomName}, 10000),
            success
        catch
            Error:Reason -> {error, Error, Reason}
        end
    end, ClientNames),
    LeaveEnd = erlang:system_time(microsecond),
    LeaveTime = LeaveEnd - LeaveStart,
    
    %% Return identical session results structure
    #{
        join_time_us => JoinTime,
        message_time_us => MessageTime,
        leave_time_us => LeaveTime,
        total_session_time_us => JoinTime + MessageTime + LeaveTime,
        message_successes => length(MessageSuccesses),
        message_failures => length(MessageFailures),
        message_latencies => MessageSuccesses,  % Contains list of individual latencies
        join_successes => length([R || R <- JoinResults, R =:= success]),
        leave_successes => length([R || R <- LeaveResults, R =:= success]),
        clients => ClientCount,
        messages_per_client => MessagesPerClient,
        total_messages => TotalMessages
    }.

%% Messaging session for a single client
run_client_messaging_session(_SystemType, ClientName, RoomName, MessagesPerClient, ParentPid) ->
    try
        lists:foreach(fun(N) ->
            try
                %% Measure individual message latency
                OpStart = erlang:system_time(microsecond),
                
                %% Send realistic message
                Message = io_lib:format("Message ~p from ~p in benchmark", [N, ClientName]),
                Result = gen_server:call(ClientName, {send_message, RoomName, Message}, 10000),
                
                OpEnd = erlang:system_time(microsecond),
                Latency = OpEnd - OpStart,
                
                case Result of
                    {ok, message_sent} -> ParentPid ! {completed, Latency};
                    _ -> ParentPid ! {failed, unexpected_result, Result}
                end
            catch
                Error:Reason -> ParentPid ! {failed, Error, Reason}
            end
        end, lists:seq(1, MessagesPerClient))
    catch
        Error:Reason -> ParentPid ! {failed, Error, Reason}
    end.

%% Collect message results
collect_message_results(0, Successes, Failures) ->
    {Successes, Failures};
collect_message_results(Remaining, Successes, Failures) ->
    receive
        {completed, Latency} ->
            collect_message_results(Remaining - 1, [Latency | Successes], Failures);
        {failed, Error, Reason} ->
            collect_message_results(Remaining - 1, Successes, [{Error, Reason} | Failures])
    after 60000 ->
        io:format("TIMEOUT: Still waiting for ~p results~n", [Remaining]),
        {Successes, Failures}
    end.

%% Fixed client name generation (consistent prefixing)
generate_baseline_client_names(ClientCount) ->
    [list_to_atom("bl_client_" ++ integer_to_list(N)) || N <- lists:seq(1, ClientCount)].

generate_instrumented_client_names(ClientCount) ->
    [list_to_atom("in_client_" ++ integer_to_list(N)) || N <- lists:seq(1, ClientCount)].

%% Standardized warmup functions
run_baseline_warmup(ClientCount, WarmupMessages) ->
    try
        start_process_safely(baseline_chat_server, fun() -> baseline_chat_server:start_link() end),
        ClientNames = generate_baseline_client_names(ClientCount),
        
        lists:foreach(fun(Name) ->
            start_process_safely(Name, fun() -> baseline_client:start_link(Name) end),
            gen_server:call(Name, register),
            gen_server:call(Name, {join_room, warmup_room})
        end, ClientNames),
        
        %% Send warmup messages
        lists:foreach(fun(ClientName) ->
            lists:foreach(fun(N) ->
                gen_server:call(ClientName, {send_message, warmup_room, 
                               io_lib:format("warmup ~p", [N])})
            end, lists:seq(1, WarmupMessages))
        end, ClientNames)
    catch
        _:_ -> ok  % Ignore warmup failures
    end.

run_instrumented_warmup(ClientCount, WarmupMessages) ->
    try
        start_process_safely(monitor, fun() -> monitor:start() end),
        start_process_safely(simple_wrapper, fun() -> simple_wrapper:start_link() end),
        start_process_safely(chat_server, fun() -> chat_server:start_link() end),
        ClientNames = generate_instrumented_client_names(ClientCount),
        
        lists:foreach(fun(Name) ->
            start_process_safely(Name, fun() -> client:start_link(Name) end),
            gen_server:call(Name, register),
            gen_server:call(Name, {join_room, warmup_room})
        end, ClientNames),
        
        %% Send warmup messages (identical pattern)
        lists:foreach(fun(ClientName) ->
            lists:foreach(fun(N) ->
                gen_server:call(ClientName, {send_message, warmup_room, 
                               io_lib:format("warmup ~p", [N])})
            end, lists:seq(1, WarmupMessages))
        end, ClientNames)
    catch
        _:_ -> ok  % Ignore warmup failures
    end.

%% Calculate session metrics
calculate_session_metrics(SystemType, SessionResult, _WallClockTime) ->
    JoinTime = maps:get(join_time_us, SessionResult),
    MessageTime = maps:get(message_time_us, SessionResult),
    LeaveTime = maps:get(leave_time_us, SessionResult),
    TotalSessionTime = maps:get(total_session_time_us, SessionResult),
    MessageSuccesses = maps:get(message_successes, SessionResult),
    TotalMessages = maps:get(total_messages, SessionResult),
    MessageLatencies = maps:get(message_latencies, SessionResult),
    
    %% Calculate throughput and latency metrics
    MessageThroughput = case MessageTime of
        0 -> 0.0;
        _ -> MessageSuccesses / (MessageTime / 1000000)  % messages per second
    end,
    
    AvgMessageLatency = case MessageLatencies of
        [] -> 0.0;
        _ -> lists:sum(MessageLatencies) / length(MessageLatencies)
    end,
    
    #{
        system_type => SystemType,
        total_session_time_us => TotalSessionTime,
        join_phase_us => JoinTime,
        message_phase_us => MessageTime,
        leave_phase_us => LeaveTime,
        message_throughput_per_sec => MessageThroughput,
        avg_message_latency_us => AvgMessageLatency,
        successful_messages => MessageSuccesses,
        total_messages => TotalMessages,
        success_rate => case TotalMessages of
            0 -> 0.0;
            _ -> (MessageSuccesses / TotalMessages) * 100
        end,
        %% Store all latencies for proper cross-iteration averaging
        all_latencies => MessageLatencies
    }.

%% Calculate comparison between baseline and instrumented
calculate_session_comparison(ClientCount, MessagesPerClient, BaselineResults, InstrumentedResults) ->
    %% Calculate averages - FIXED to properly handle latencies
    AvgBaseline = average_metrics_corrected(BaselineResults),
    AvgInstrumented = average_metrics_corrected(InstrumentedResults),
    
    %% Calculate overhead percentages
    TotalOverhead = calculate_overhead_percent(
        maps:get(total_session_time_us, AvgBaseline),
        maps:get(total_session_time_us, AvgInstrumented)
    ),
    
    MessageOverhead = calculate_overhead_percent(
        maps:get(message_phase_us, AvgBaseline),
        maps:get(message_phase_us, AvgInstrumented)
    ),
    
    %% Calculate coefficient of variation for stability analysis
    BaselineStability = calculate_cv([maps:get(total_session_time_us, R) || R <- BaselineResults]),
    InstrumentedStability = calculate_cv([maps:get(total_session_time_us, R) || R <- InstrumentedResults]),
    
    %% Print comparison
    io:format("  Results: Baseline ~.1f ms | Instrumented ~.1f ms | Overhead: ~.1f%%~n",
             [maps:get(total_session_time_us, AvgBaseline)/1000,
              maps:get(total_session_time_us, AvgInstrumented)/1000,
              TotalOverhead]),
    
    #{
        clients => ClientCount,
        messages_per_client => MessagesPerClient,
        total_messages => ClientCount * MessagesPerClient,
        iterations => length(BaselineResults),
        baseline => AvgBaseline,
        instrumented => AvgInstrumented,
        total_overhead_percent => TotalOverhead,
        message_overhead_percent => MessageOverhead,
        baseline_stability_cv => BaselineStability,
        instrumented_stability_cv => InstrumentedStability,
        raw_baseline_results => BaselineResults,
        raw_instrumented_results => InstrumentedResults
    }.

%% CORRECTED: Helper function that properly averages latencies across all iterations
average_metrics_corrected(Results) ->
    N = length(Results),
    SumFun = fun(Key) -> lists:sum([maps:get(Key, R) || R <- Results]) / N end,
    
    %% Collect ALL individual latencies from ALL iterations
    AllLatencies = lists:flatten([maps:get(all_latencies, R) || R <- Results]),
    
    %% Calculate proper average latency from all individual measurements
    AvgLatency = case AllLatencies of
        [] -> 0.0;
        _ -> lists:sum(AllLatencies) / length(AllLatencies)
    end,
    
    #{
        total_session_time_us => SumFun(total_session_time_us),
        join_phase_us => SumFun(join_phase_us),
        message_phase_us => SumFun(message_phase_us),
        leave_phase_us => SumFun(leave_phase_us),
        message_throughput_per_sec => SumFun(message_throughput_per_sec),
        avg_message_latency_us => AvgLatency,  % CORRECTED: Use properly calculated average
        successful_messages => round(SumFun(successful_messages))
    }.

calculate_overhead_percent(BaselineTime, InstrumentedTime) ->
    case BaselineTime of
        0 -> 0.0;
        _ -> ((InstrumentedTime - BaselineTime) / BaselineTime) * 100
    end.

calculate_cv(Values) ->
    case Values of
        [] -> 0.0;
        [_] -> 0.0;
        _ ->
            Mean = lists:sum(Values) / length(Values),
            Variance = lists:sum([(X - Mean) * (X - Mean) || X <- Values]) / (length(Values) - 1),
            StdDev = math:sqrt(Variance),
            case Mean of
                0.0 -> 0.0;
                _ -> StdDev / Mean
            end
    end.

%% Process management
start_process_safely(ProcessName, StartFun) ->
    case whereis(ProcessName) of
        undefined ->
            try
                StartFun()
            catch
                _:_ -> {error, start_failed}
            end;
        _Pid ->
            {ok, already_running}
    end.

%% System cleanup and stabilization
complete_system_cleanup() ->
    %% Stop all processes in correct order
    AllProcesses = lists:flatten([
        get_client_processes(),
        [baseline_chat_server, baseline_client_registry,
         chat_server, client_registry, simple_wrapper]
    ]),
    
    lists:foreach(fun(Name) ->
        case whereis(Name) of
            undefined -> ok;
            Pid -> 
                try gen_server:stop(Name, normal, 1000)
                catch _:_ -> 
                    try exit(Pid, kill)
                    catch _:_ -> ok
                    end
                end
        end
    end, AllProcesses),
    
    %% Stop monitor last
    catch monitor:stop(),
    
    %% Garbage collection
    erlang:garbage_collect().

get_client_processes() ->
    [Name || Name <- registered(),
             is_client_process_name(atom_to_list(Name))].

is_client_process_name(NameStr) ->
    lists:prefix("bl_client_", NameStr) orelse 
    lists:prefix("in_client_", NameStr).

system_stabilization_wait() ->
    timer:sleep(500),  % Allow process cleanup to complete
    erlang:garbage_collect().

%% Report generation
generate_final_report(Results, TotalBenchmarkTime) ->
    io:format("~n=== FINAL BENCHMARK REPORT ===~n"),
    io:format("Total benchmark time: ~p ms~n~n", [TotalBenchmarkTime]),
    
    lists:foreach(fun(TestResult) ->
        ClientCount = maps:get(clients, TestResult),
        MessagesPerClient = maps:get(messages_per_client, TestResult),
        TotalOverhead = maps:get(total_overhead_percent, TestResult),
        MessageOverhead = maps:get(message_overhead_percent, TestResult),
        BaselineStability = maps:get(baseline_stability_cv, TestResult),
        InstrumentedStability = maps:get(instrumented_stability_cv, TestResult),
        
        io:format("~p clients × ~p messages:~n", [ClientCount, MessagesPerClient]),
        io:format("  Total overhead: ~.1f%%~n", [TotalOverhead]),
        io:format("  Message overhead: ~.1f%%~n", [MessageOverhead]),
        io:format("  Stability (CV): Baseline ~.1f%%, Instrumented ~.1f%%~n~n",
                 [BaselineStability * 100, InstrumentedStability * 100])
    end, Results).

save_results_to_csv(Results, TotalTime) ->
    Header = "Clients,Messages_Per_Client,Total_Messages,System_Type,Total_Time_ms,Message_Time_ms,Join_Time_ms,Leave_Time_ms,Throughput_msg_per_sec,Avg_Latency_us,Overhead_Percent,CV_Percent,Iterations~n",
    
    CsvData = lists:map(fun(TestResult) ->
        ClientCount = maps:get(clients, TestResult),
        MessagesPerClient = maps:get(messages_per_client, TestResult),
        TotalMessages = maps:get(total_messages, TestResult),
        Iterations = maps:get(iterations, TestResult),
        Baseline = maps:get(baseline, TestResult),
        Instrumented = maps:get(instrumented, TestResult),
        TotalOverhead = maps:get(total_overhead_percent, TestResult),
        BaselineCV = maps:get(baseline_stability_cv, TestResult),
        InstrumentedCV = maps:get(instrumented_stability_cv, TestResult),
        
        BaselineRow = io_lib:format("~p,~p,~p,Baseline,~.3f,~.3f,~.3f,~.3f,~.3f,~.3f,0.0,~.3f,~p~n", [
            ClientCount, MessagesPerClient, TotalMessages,
            maps:get(total_session_time_us, Baseline) / 1000,
            maps:get(message_phase_us, Baseline) / 1000,
            maps:get(join_phase_us, Baseline) / 1000,
            maps:get(leave_phase_us, Baseline) / 1000,
            maps:get(message_throughput_per_sec, Baseline),
            maps:get(avg_message_latency_us, Baseline),
            BaselineCV * 100,
            Iterations
        ]),
        
        InstrumentedRow = io_lib:format("~p,~p,~p,Instrumented,~.3f,~.3f,~.3f,~.3f,~.3f,~.3f,~.3f,~.3f,~p~n", [
            ClientCount, MessagesPerClient, TotalMessages,
            maps:get(total_session_time_us, Instrumented) / 1000,
            maps:get(message_phase_us, Instrumented) / 1000,
            maps:get(join_phase_us, Instrumented) / 1000,
            maps:get(leave_phase_us, Instrumented) / 1000,
            maps:get(message_throughput_per_sec, Instrumented),
            maps:get(avg_message_latency_us, Instrumented),
            TotalOverhead,
            InstrumentedCV * 100,
            Iterations
        ]),
        
        [BaselineRow, InstrumentedRow]
    end, Results),
    
    file:write_file("chat_benchmark_results.csv", [Header, lists:flatten(CsvData)]),
    io:format("Results saved to: chat_benchmark_results.csv~n").

cleanup() ->
    complete_system_cleanup().