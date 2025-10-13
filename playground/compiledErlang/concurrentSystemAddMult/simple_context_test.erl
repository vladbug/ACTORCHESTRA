-module(simple_context_test).
-export([test_basic_context/0, test_with_wrapper/0, run_all_tests/0]).

%% Basic test to see if context is being injected
test_basic_context() ->
    io:format("~n=== BASIC CONTEXT TEST ===~n"),
    
    %% Start system
    case whereis(slow_central_server) of
        undefined -> 
            {ok, _} = slow_central_server:start_link();
        _ -> ok
    end,
    
    %% Create a client
    {ok, _ClientPid} = client:start_link(test_client),
    
    %% Connect and test
    ok = client:connect(test_client),
    
    TestNumber = 25,
    Expected = (TestNumber + 10) * 2,  % 25 + 10 = 35, 35 * 2 = 70
    
    io:format("Sending number: ~p~n", [TestNumber]),
    io:format("Expected result: ~p~n", [Expected]),
    
    StartTime = erlang:system_time(millisecond),
    Result = client:send_number(test_client, TestNumber),
    EndTime = erlang:system_time(millisecond),
    
    io:format("Got result: ~p in ~p ms~n", [Result, EndTime - StartTime]),
    
    case Result of
        {ok, Expected} ->
            io:format("✓ BASIC TEST PASSED~n");
        {ok, Other} ->
            io:format("✗ BASIC TEST FAILED: Expected ~p, got ~p~n", [Expected, Other]);
        Error ->
            io:format("✗ BASIC TEST ERROR: ~p~n", [Error])
    end,
    
    client:disconnect(test_client),
    ok.

%% Test using the wrapper for context propagation
test_with_wrapper() ->
    io:format("~n=== WRAPPER CONTEXT TEST ===~n"),
    
    %% Start system including wrapper
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
    
    %% Create a client
    {ok, _ClientPid} = client:start_link(wrapper_test_client),
    ok = client:connect(wrapper_test_client),
    
    %% Create a unique context
    Context = make_ref(),
    TestNumber = 33,
    Expected = (TestNumber + 10) * 2,  % 33 + 10 = 43, 43 * 2 = 86
    
    io:format("Using context: ~p~n", [Context]),
    io:format("Sending number: ~p~n", [TestNumber]),
    io:format("Expected result: ~p~n", [Expected]),
    
    StartTime = erlang:system_time(millisecond),
    
    %% Use wrapper to send with context
    Result = wrapper:call(wrapper_test_client, {send_number, TestNumber}, Context),
    
    EndTime = erlang:system_time(millisecond),
    
    io:format("Got result: ~p in ~p ms~n", [Result, EndTime - StartTime]),
    
    case Result of
        {ok, Expected} ->
            io:format("✓ WRAPPER TEST PASSED~n");
        {ok, Other} ->
            io:format("✗ WRAPPER TEST FAILED: Expected ~p, got ~p~n", [Expected, Other]);
        Error ->
            io:format("✗ WRAPPER TEST ERROR: ~p~n", [Error])
    end,
    
    client:disconnect(wrapper_test_client),
    ok.

%% Test multiple concurrent operations
test_concurrent_operations() ->
    io:format("~n=== CONCURRENT OPERATIONS TEST ===~n"),
    
    %% Start system
    case whereis(slow_central_server) of
        undefined -> 
            {ok, _} = slow_central_server:start_link();
        _ -> ok
    end,
    
    %% Create multiple clients
    NumClients = 3,
    ClientNames = [list_to_atom("client_" ++ integer_to_list(N)) || N <- lists:seq(1, NumClients)],
    
    %% Start all clients
    lists:foreach(fun(ClientName) ->
        {ok, _} = client:start_link(ClientName),
        ok = client:connect(ClientName)
    end, ClientNames),
    
    %% Prepare test data
    TestNumbers = [10, 20, 30],
    Expected = [(N + 10) * 2 || N <- TestNumbers],
    
    io:format("Starting ~p concurrent operations...~n", [NumClients]),
    io:format("Test numbers: ~p~n", [TestNumbers]),
    io:format("Expected results: ~p~n", [Expected]),
    
    %% Start all operations concurrently
    StartTime = erlang:system_time(millisecond),
    
    TestPids = lists:map(fun({ClientName, TestNumber}) ->
        spawn(fun() ->
            io:format("Client ~p processing ~p~n", [ClientName, TestNumber]),
            Result = client:send_number(ClientName, TestNumber),
            EndTime = erlang:system_time(millisecond),
            io:format("Client ~p got result: ~p at ~p ms~n", [ClientName, Result, EndTime - StartTime])
        end)
    end, lists:zip(ClientNames, TestNumbers)),
    
    %% Wait for all to complete
    lists:foreach(fun(Pid) ->
        Ref = monitor(process, Pid),
        receive
            {'DOWN', Ref, process, Pid, _Reason} -> ok
        after 15000 ->
            io:format("WARNING: Test process ~p timed out~n", [Pid])
        end
    end, TestPids),
    
    EndTime = erlang:system_time(millisecond),
    io:format("All concurrent operations completed in ~p ms~n", [EndTime - StartTime]),
    
    %% Cleanup
    lists:foreach(fun(ClientName) ->
        client:disconnect(ClientName)
    end, ClientNames),
    
    io:format("✓ CONCURRENT TEST COMPLETED~n"),
    ok.

%% Run all tests
run_all_tests() ->
    io:format("~n==========================================~n"),
    io:format("RUNNING CONTEXT PRESERVATION TEST SUITE~n"),
    io:format("==========================================~n"),
    
    test_basic_context(),
    timer:sleep(1000),
    
    test_with_wrapper(),
    timer:sleep(1000),
    
    test_concurrent_operations(),
    
    io:format("~n==========================================~n"),
    io:format("ALL TESTS COMPLETED~n"),
    io:format("==========================================~n"),
    
    ok.
