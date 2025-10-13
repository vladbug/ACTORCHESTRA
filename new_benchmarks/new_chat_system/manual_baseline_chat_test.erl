-module(manual_baseline_chat_test).
-export([run_test/0, run_simple_test/0, run_complex_test/0, run_session_test/0, cleanup/0, test_quick/0, test_messages/0, test_session/0, test_multi/0]).

%% Simple manual test - single message
run_simple_test() ->
    io:format("=== MANUAL BASELINE CHAT TEST - SINGLE MESSAGE ===~n"),
    
    %% Setup
    {ok, _} = baseline_chat_server:start_link(),
    {ok, _} = baseline_client:start_link(test_client),
    gen_server:call(test_client, register),
    
    %% Join room
    gen_server:call(test_client, {join_room, test_room}),
    
    %% Single timed message operation
    io:format("Sending single message...~n"),
    StartTime = erlang:system_time(microsecond),
    
    Result = gen_server:call(test_client, {send_message, test_room, "Hello, this is a test message!"}, 10000),
    
    EndTime = erlang:system_time(microsecond),
    ElapsedTime = EndTime - StartTime,
    
    %% Results
    io:format("Result: ~p~n", [Result]),
    io:format("Time: ~p microseconds (~.2f milliseconds)~n", [ElapsedTime, ElapsedTime/1000]),
    
    %% Leave room and cleanup
    gen_server:call(test_client, {leave_room, test_room}),
    cleanup(),
    
    ElapsedTime.

%% Complex manual test - multiple messages
run_complex_test() ->
    io:format("=== MANUAL BASELINE CHAT TEST - MULTIPLE MESSAGES ===~n"),
    
    %% Setup
    {ok, _} = baseline_chat_server:start_link(),
    {ok, _} = baseline_client:start_link(test_client),
    gen_server:call(test_client, register),
    gen_server:call(test_client, {join_room, test_room}),
    
    %% Complex workload - 50 messages
    NumberOfMessages = 50,
    io:format("Sending ~p messages sequentially...~n", [NumberOfMessages]),
    
    StartTime = erlang:system_time(microsecond),
    
    %% Send messages sequentially
    Results = lists:map(fun(N) ->
        Message = io_lib:format("Test message number ~p from baseline client", [N]),
        gen_server:call(test_client, {send_message, test_room, Message}, 10000)
    end, lists:seq(1, NumberOfMessages)),
    
    EndTime = erlang:system_time(microsecond),
    ElapsedTime = EndTime - StartTime,
    
    %% Results
    SuccessCount = length([R || R <- Results, R =:= {ok, message_sent}]),
    AvgTimePerMessage = ElapsedTime / NumberOfMessages,
    
    io:format("Completed: ~p/~p messages successful~n", [SuccessCount, NumberOfMessages]),
    io:format("Total time: ~p microseconds (~.2f milliseconds)~n", [ElapsedTime, ElapsedTime/1000]),
    io:format("Average per message: ~.2f microseconds (~.3f milliseconds)~n", 
             [AvgTimePerMessage, AvgTimePerMessage/1000]),
    io:format("Message rate: ~.2f messages/second~n", [NumberOfMessages / (ElapsedTime / 1000000)]),
    
    %% Leave room and cleanup
    gen_server:call(test_client, {leave_room, test_room}),
    cleanup(),
    
    #{
        total_time_us => ElapsedTime,
        messages => NumberOfMessages,
        success_count => SuccessCount,
        avg_time_per_message_us => AvgTimePerMessage,
        message_rate_per_sec => NumberOfMessages / (ElapsedTime / 1000000)
    }.

%% Session test - complete chat session (join → messages → leave)
run_session_test() ->
    io:format("=== MANUAL BASELINE CHAT TEST - COMPLETE SESSION ===~n"),
    
    %% Setup
    {ok, _} = baseline_chat_server:start_link(),
    {ok, _} = baseline_client:start_link(test_client),
    gen_server:call(test_client, register),
    
    NumberOfMessages = 20,
    io:format("Running complete chat session: join → ~p messages → leave~n", [NumberOfMessages]),
    
    %% Measure complete session
    SessionStart = erlang:system_time(microsecond),
    
    %% Phase 1: Join room
    JoinStart = erlang:system_time(microsecond),
    gen_server:call(test_client, {join_room, test_room}),
    JoinEnd = erlang:system_time(microsecond),
    JoinTime = JoinEnd - JoinStart,
    
    %% Phase 2: Send messages
    MessagingStart = erlang:system_time(microsecond),
    MessageResults = lists:map(fun(N) ->
        Message = io_lib:format("Session message ~p: Hello everyone!", [N]),
        gen_server:call(test_client, {send_message, test_room, Message}, 10000)
    end, lists:seq(1, NumberOfMessages)),
    MessagingEnd = erlang:system_time(microsecond),
    MessagingTime = MessagingEnd - MessagingStart,
    
    %% Phase 3: Leave room  
    LeaveStart = erlang:system_time(microsecond),
    gen_server:call(test_client, {leave_room, test_room}),
    LeaveEnd = erlang:system_time(microsecond),
    LeaveTime = LeaveEnd - LeaveStart,
    
    SessionEnd = erlang:system_time(microsecond),
    TotalSessionTime = SessionEnd - SessionStart,
    
    %% Results
    MessageSuccesses = length([R || R <- MessageResults, R =:= {ok, message_sent}]),
    
    io:format("Session completed successfully!~n"),
    io:format("Join time: ~p μs (~.2f ms)~n", [JoinTime, JoinTime/1000]),
    io:format("Messaging time: ~p μs (~.2f ms) - ~p/~p messages~n", 
             [MessagingTime, MessagingTime/1000, MessageSuccesses, NumberOfMessages]),
    io:format("Leave time: ~p μs (~.2f ms)~n", [LeaveTime, LeaveTime/1000]),
    io:format("Total session time: ~p μs (~.2f ms)~n", [TotalSessionTime, TotalSessionTime/1000]),
    io:format("Average per message: ~.2f μs~n", [MessagingTime / NumberOfMessages]),
    
    cleanup(),
    
    #{
        total_session_time_us => TotalSessionTime,
        join_time_us => JoinTime,
        messaging_time_us => MessagingTime,
        leave_time_us => LeaveTime,
        messages => NumberOfMessages,
        message_successes => MessageSuccesses,
        avg_message_time_us => MessagingTime / NumberOfMessages
    }.

%% Multi-client concurrent test
run_multi_client_test() ->
    io:format("=== MANUAL BASELINE CHAT TEST - MULTI-CLIENT SESSION ===~n"),
    
    %% Setup
    {ok, _} = baseline_chat_server:start_link(),
    
    NumClients = 20,
    MessagesPerClient = 1000,
    ClientNames = [baseline_client1, baseline_client2, baseline_client3, baseline_client4, baseline_client5,
                   baseline_client6, baseline_client7, baseline_client8,
                   baseline_client9, baseline_client10, baseline_client11, baseline_client12,
                   baseline_client13, baseline_client14, baseline_client15,
                   baseline_client16, baseline_client17, baseline_client18,
                   baseline_client19, baseline_client20],
    
    %% Start and register all clients
    lists:foreach(fun(Name) ->
        {ok, _} = baseline_client:start_link(Name),
        gen_server:call(Name, register),
        gen_server:call(Name, {join_room, multi_test_room})
    end, ClientNames),
    
    io:format("~p clients sending ~p messages each concurrently...~n", [NumClients, MessagesPerClient]),
    
    %% Concurrent messaging
    StartTime = erlang:system_time(microsecond),
    ParentPid = self(),
    
    lists:foreach(fun(ClientName) ->
        spawn(fun() ->
            ClientResults = lists:map(fun(N) ->
                Message = io_lib:format("Message ~p from ~p", [N, ClientName]),
                MsgStart = erlang:system_time(microsecond),
                Result = gen_server:call(ClientName, {send_message, multi_test_room, Message}, 10000),
                MsgEnd = erlang:system_time(microsecond),
                {Result, MsgEnd - MsgStart}
            end, lists:seq(1, MessagesPerClient)),
            ParentPid ! {client_done, ClientName, ClientResults}
        end)
    end, ClientNames),
    
    %% Collect results from all clients
    AllResults = collect_client_results(NumClients, []),
    
    EndTime = erlang:system_time(microsecond),
    TotalTime = EndTime - StartTime,
    
    %% Analyze results
    {TotalMessages, TotalSuccesses, AllLatencies} = lists:foldl(
        fun({_ClientName, ClientResults}, {MsgAcc, SuccAcc, LatAcc}) ->
            ClientSuccesses = length([R || {R, _} <- ClientResults, R =:= {ok, message_sent}]),
            ClientLatencies = [Lat || {R, Lat} <- ClientResults, R =:= {ok, message_sent}],
            {MsgAcc + length(ClientResults), SuccAcc + ClientSuccesses, ClientLatencies ++ LatAcc}
        end, {0, 0, []}, AllResults),
    
    AvgLatency = case AllLatencies of
        [] -> 0.0;
        _ -> lists:sum(AllLatencies) / length(AllLatencies)
    end,
    
    io:format("Multi-client test completed!~n"),
    io:format("Total time: ~p μs (~.2f ms)~n", [TotalTime, TotalTime/1000]),
    io:format("Messages: ~p sent, ~p successful~n", [TotalMessages, TotalSuccesses]),
    io:format("Average message latency: ~.2f μs~n", [AvgLatency]),
    io:format("Overall throughput: ~.2f messages/sec~n", [TotalSuccesses / (TotalTime / 1000000)]),
    
    %% Leave room and cleanup
    lists:foreach(fun(Name) ->
        gen_server:call(Name, {leave_room, multi_test_room})
    end, ClientNames),
    cleanup(),
    
    #{
        total_time_us => TotalTime,
        total_messages => TotalMessages,
        successful_messages => TotalSuccesses,
        avg_message_latency_us => AvgLatency,
        throughput_msg_per_sec => TotalSuccesses / (TotalTime / 1000000),
        clients => NumClients
    }.

%% Helper to collect results from multiple clients
collect_client_results(0, Acc) ->
    Acc;
collect_client_results(Remaining, Acc) ->
    receive
        {client_done, ClientName, Results} ->
            collect_client_results(Remaining - 1, [{ClientName, Results} | Acc])
    after 30000 ->
        io:format("TIMEOUT: Still waiting for ~p clients~n", [Remaining]),
        Acc
    end.

%% Default test runner
run_test() ->
    run_session_test().

%% Cleanup function
cleanup() ->
    ProcessesToStop = [
        test_client,
        baseline_client1, baseline_client2, baseline_client3, baseline_client4, baseline_client5,
                   baseline_client6, baseline_client7, baseline_client8,
                   baseline_client9, baseline_client10, baseline_client11, baseline_client12,
                   baseline_client13, baseline_client14, baseline_client15,
                   baseline_client16, baseline_client17, baseline_client18,
                   baseline_client19, baseline_client20,
        baseline_chat_server,
        baseline_client_registry
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
    
    timer:sleep(100),
    erlang:garbage_collect().

%% Quick test functions for manual execution
test_quick() ->
    io:format("Quick baseline chat test:~n"),
    Time = run_simple_test(),
    io:format("~n=== RESULT: ~p microseconds ===~n", [Time]).

test_messages() ->
    io:format("Message batch baseline chat test:~n"),
    Result = run_complex_test(),
    io:format("~n=== RESULT: ~.2f ms total, ~.2f μs per message ===~n", 
             [maps:get(total_time_us, Result)/1000, maps:get(avg_time_per_message_us, Result)]).

test_session() ->
    io:format("Complete session baseline chat test:~n"),
    Result = run_session_test(),
    io:format("~n=== RESULT: ~.2f ms session, ~.2f μs avg per message ===~n", 
             [maps:get(total_session_time_us, Result)/1000, maps:get(avg_message_time_us, Result)]).

test_multi() ->
    io:format("Multi-client baseline chat test:~n"),
    Result = run_multi_client_test(),
    io:format("~n=== RESULT: ~.2f ms total, ~.2f μs avg latency, ~.1f msg/sec ===~n", 
             [maps:get(total_time_us, Result)/1000, maps:get(avg_message_latency_us, Result),
              maps:get(throughput_msg_per_sec, Result)]).