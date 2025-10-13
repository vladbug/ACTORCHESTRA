-module(simple_test).
-export([run_test/0, test_single_request/0, test_concurrent_requests/0, cleanup/0]).

%% Main test runner
run_test() ->
    io:format("=== Starting Simple Context Injection Test ===~n"),
    io:format("This test demonstrates the new {with_context, Context, Msg} approach~n"),
    io:format("Expected flow: client -> add_server -> mul_server -> add_server -> client~n"),
    io:format("Processing chain: Number -> +10 -> *2 -> Final Result~n~n"),
    
    %% Start all system components
    setup_system(),
    
    %% Run single request test
    io:format("~n=== Test 1: Single Request ===~n"),
    test_single_request(),
    
    timer:sleep(2000), % Wait for processing to complete
    
    %% Run concurrent requests test
    io:format("~n=== Test 2: Concurrent Requests (Context Separation) ===~n"),
    test_concurrent_requests(),
    
    timer:sleep(3000), % Wait for all processing to complete
    
    %% Cleanup
    cleanup(),
    
    io:format("~n=== Simple Context Injection Test Complete ===~n").

%% Setup all system components
setup_system() ->
    io:format("Setting up simple system components...~n"),
    
    %% Start the wrapper first (required for context injection)
    {ok, _} = simple_wrapper:start_link(),
    
    %% Start the processing servers
    {ok, _} = add_server:start_link(),
    {ok, _} = mul_server:start_link(),
    
    %% Start monitor for context verification
    {ok, _} = simple_monitor:start(),
    
    %% Start client instances
    {ok, _} = simple_client:start_link(client1),
    {ok, _} = simple_client:start_link(client2),
    
    timer:sleep(100), % Give processes time to start
    io:format("Simple system setup complete.~n").

%% Test single request to verify basic functionality
test_single_request() ->
    io:format("Testing single request: client1 sends number 5~n"),
    io:format("Expected result: 5 -> +10 -> 15 -> *2 -> 30~n"),
    
    StartTime = erlang:system_time(millisecond),
    
    Result = simple_client:send_number(client1, 5),
    
    EndTime = erlang:system_time(millisecond),
    TotalTime = EndTime - StartTime,
    
    case Result of
        {ok, 30} ->
            io:format("✓ Single request SUCCESS: Got expected result 30 in ~p ms~n", [TotalTime]);
        Other ->
            io:format("✗ Single request FAILED: Got ~p, expected {ok, 30}~n", [Other])
    end,
    
    io:format("Check monitor output above for context preservation verification.~n").

%% Test concurrent requests to verify context separation
test_concurrent_requests() ->
    io:format("Testing concurrent requests to verify context separation~n"),
    io:format("client1: 3 -> +10 -> 13 -> *2 -> 26~n"),
    io:format("client2: 7 -> +10 -> 17 -> *2 -> 34~n"),
    io:format("Both should process with separate contexts~n~n"),
    
    StartTime = erlang:system_time(millisecond),
    
    %% Send requests concurrently
    Pid1 = spawn(fun() ->
        io:format("[TEST] Starting client1 request (3)...~n"),
        Result1 = simple_client:send_number(client1, 3),
        io:format("[TEST] Client1 result: ~p (expected: {ok, 26})~n", [Result1])
    end),
    
    %% Small delay to create some overlap
    timer:sleep(50),
    
    Pid2 = spawn(fun() ->
        io:format("[TEST] Starting client2 request (7)...~n"),
        Result2 = simple_client:send_number(client2, 7),
        io:format("[TEST] Client2 result: ~p (expected: {ok, 34})~n", [Result2])
    end),
    
    %% Wait for both to complete
    wait_for_pids([Pid1, Pid2]),
    
    EndTime = erlang:system_time(millisecond),
    TotalTime = EndTime - StartTime,
    
    io:format("~nConcurrent requests completed in ~p ms~n", [TotalTime]),
    io:format("Check the monitor output above to verify:~n"),
    io:format("  1. Two different contexts were used~n"),
    io:format("  2. Each context was preserved throughout its entire chain~n"),
    io:format("  3. No context mixing occurred between requests~n").

%% Utility function to wait for multiple processes to complete
wait_for_pids([]) ->
    ok;
wait_for_pids([Pid | Rest]) ->
    case is_process_alive(Pid) of
        true ->
            timer:sleep(50),
            wait_for_pids([Pid | Rest]);
        false ->
            wait_for_pids(Rest)
    end.

%% Cleanup all processes
cleanup() ->
    io:format("Cleaning up simple system...~n"),
    
    %% Stop monitor
    catch simple_monitor:stop(),
    
    %% Stop clients
    stop_process(client1),
    stop_process(client2),
    
    %% Stop server components
    stop_process(add_server),
    stop_process(mul_server),
    
    %% Stop wrapper
    stop_process(simple_wrapper),
    
    io:format("Simple cleanup complete.~n").

%% Helper to stop a registered process
stop_process(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            try
                gen_server:stop(Name, normal, 1000)
            catch
                _:_ -> 
                    exit(Pid, kill)
            end
    end.

%% Additional helper functions for manual testing

%% Test the context injection pattern directly
test_context_injection() ->
    setup_system(),
    
    io:format("=== Testing Context Injection Pattern Directly ===~n"),
    
    %% Test direct call with context injection
    TestContext = make_ref(),
    io:format("Sending {with_context, ~p, {add_ten, 42}} directly to add_server~n", [TestContext]),
    
    Result = gen_server:call(add_server, {with_context, TestContext, {add_ten, 42}}),
    io:format("Direct context injection result: ~p~n", [Result]),
    
    cleanup().

%% Test monitor separately
test_monitor_only() ->
    {ok, _} = simple_monitor:start(),
    
    io:format("=== Testing Monitor Message Handling ===~n"),
    
    %% Send test messages to monitor
    TestContext = make_ref(),
    monitor ! {simple_client, add_server, {add_ten, 5}, TestContext},
    monitor ! {add_server, mul_server, {multiply_two, 15}, TestContext},
    monitor ! {mul_server, add_server, {ok, 30}, TestContext},
    monitor ! {add_server, simple_client, {ok, 30}, TestContext},
    
    timer:sleep(1000),
    simple_monitor:stop().