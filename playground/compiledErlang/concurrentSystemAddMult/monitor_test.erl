-module(monitor_test).
-export([test_monitor_flow/0, test_multiple_flows/0, start_system/0]).

%% Test the monitor with the wrapper
test_monitor_flow() ->
    io:format("~n=== MONITOR FLOW TEST ===~n"),
    
    %% Start all system components
    start_system(),
    
    %% Start the monitor
    monitor:start(),
    
    %% Create a client
    {ok, _ClientPid} = client:start_link(monitor_test_client),
    ok = client:connect(monitor_test_client),
    
    %% Create a context for testing
    Context = make_ref(),
    TestNumber = 42,
    Expected = (TestNumber + 10) * 2,  % 42 + 10 = 52, 52 * 2 = 104
    
    io:format("Test Context: ~p~n", [Context]),
    io:format("Test Number: ~p~n", [TestNumber]),
    io:format("Expected Result: ~p~n", [Expected]),
    
    %% Use wrapper to make the call - this should trigger monitor notifications
    io:format("Making call through wrapper...~n"),
    StartTime = erlang:system_time(millisecond),
    Result = wrapper:call(monitor_test_client, {send_number, TestNumber}, Context),
    EndTime = erlang:system_time(millisecond),
    
    io:format("Result: ~p (took ~p ms)~n", [Result, EndTime - StartTime]),
    
    %% Wait a moment for monitor to process messages
    timer:sleep(1000),
    
    %% Verify the result
    case Result of
        {ok, Expected} ->
            io:format("✓ TEST PASSED - Correct result and monitor should show context flow~n");
        {ok, Other} ->
            io:format("✗ TEST FAILED - Expected ~p, got ~p~n", [Expected, Other]);
        Error ->
            io:format("✗ TEST ERROR - ~p~n", [Error])
    end,
    
    %% Cleanup
    client:disconnect(monitor_test_client),
    io:format("=== MONITOR TEST COMPLETE ===~n~n"),
    
    %% Suggest next test
    io:format("Try running multiple tests:~n"),
    io:format("monitor_test:test_multiple_flows().~n"),
    ok.

%% Test multiple concurrent flows
test_multiple_flows() ->
    io:format("~n=== MULTIPLE MONITOR FLOWS TEST ===~n"),
    
    start_system(),
    monitor:start(),
    
    %% Create multiple clients
    NumTests = 3,
    TestNumbers = [10, 20, 30],
    
    ClientNames = [list_to_atom("monitor_client_" ++ integer_to_list(N)) || N <- lists:seq(1, NumTests)],
    
    %% Start all clients
    lists:foreach(fun(ClientName) ->
        {ok, _} = client:start_link(ClientName),
        ok = client:connect(ClientName)
    end, ClientNames),
    
    %% Create contexts and test data
    Contexts = [make_ref() || _ <- TestNumbers],
    TestData = lists:zip3(Contexts, TestNumbers, ClientNames),
    
    io:format("Starting ~p concurrent flows with contexts:~n", [NumTests]),
    lists:foreach(fun({Ctx, Num, _Client}) ->
        io:format("  Context ~p for number ~p~n", [Ctx, Num])
    end, TestData),
    
    %% Launch all tests concurrently
    StartTime = erlang:system_time(millisecond),
    
    TestPids = lists:map(fun({Context, Number, ClientName}) ->
        spawn(fun() ->
            io:format("Starting flow: Context ~p, Number ~p, Client ~p~n", 
                      [Context, Number, ClientName]),
            
            Result = wrapper:call(ClientName, {send_number, Number}, Context),
            Expected = (Number + 10) * 2,
            
            case Result of
                {ok, Expected} ->
                    io:format("✓ Flow ~p PASSED: ~p -> ~p~n", [Number, Number, Expected]);
                {ok, Other} ->
                    io:format("✗ Flow ~p FAILED: Expected ~p, got ~p~n", [Number, Expected, Other]);
                Error ->
                    io:format("✗ Flow ~p ERROR: ~p~n", [Number, Error])
            end
        end)
    end, TestData),
    
    %% Wait for all tests to complete
    lists:foreach(fun(Pid) ->
        Ref = monitor(process, Pid),
        receive
            {'DOWN', Ref, process, Pid, _Reason} -> ok
        after 15000 ->
            io:format("WARNING: Test process ~p timed out~n", [Pid])
        end
    end, TestPids),
    
    EndTime = erlang:system_time(millisecond),
    io:format("All flows completed in ~p ms~n", [EndTime - StartTime]),
    
    %% Wait for monitor to process all messages
    timer:sleep(2000),
    
    %% Cleanup
    lists:foreach(fun(ClientName) ->
        client:disconnect(ClientName)
    end, ClientNames),
    
    io:format("=== MULTIPLE FLOWS TEST COMPLETE ===~n"),
    io:format("Check the monitor output above to verify context preservation!~n"),
    ok.

%% Helper to start all system components
start_system() ->
    case whereis(slow_central_server) of
        undefined -> 
            {ok, _} = slow_central_server:start_link();
        _ -> ok
    end,
    
    case whereis(wrapper) of
        undefined ->
            {ok, _} = wrapper:start_link();
        _ -> ok
    end,
    
    ok.
