-module(test_async_clients).
-export([run_async_test/0, test_interleaved_processing/0, cleanup/0]).

%% Main async test runner
run_async_test() ->
    io:format("=== Starting Asynchronous Two Clients Test ===~n"),
    io:format("This test demonstrates context tracking with interleaved processing~n"),
    
    %% Start all system components
    setup_system(),
    
    %% Run interleaved processing test
    io:format("~n=== Test: Interleaved Async Processing ===~n"),
    test_interleaved_processing(),
    
    timer:sleep(5000), % Wait for all async operations to complete
    
    %% Cleanup
    cleanup(),
    
    io:format("~n=== Async Two Clients Test Complete ===~n").

%% Setup all system components
setup_system() ->
    io:format("Setting up async system components...~n"),
    
    %% Start concurrency logger first
    {ok, _} = concurrency_logger:start(),
    
    %% Start the simple_wrapper first (required for context injection)
    {ok, _} = simple_wrapper:start_link(),
    
    %% Start the ASYNC processing components
    {ok, _} = slow_central_server:start_link(),
    
    %% Start monitor for context verification
    {ok, _} = monitor:start(),
    
    %% Start two client instances
    {ok, _} = client:start_link(client1),
    {ok, _} = client:start_link(client2),
    
    timer:sleep(100), % Give processes time to start
    io:format("Async system setup complete.~n").

%% Test interleaved processing - this should show context separation
test_interleaved_processing() ->
    io:format("Testing interleaved async processing...~n"),
    io:format("Expected interleaving pattern (check monitor output):~n"),
    io:format("  c1 -> central_server~n"),
    io:format("  c2 -> central_server~n"),
    io:format("  c2 -> proc_add10 (if c2 processes faster)~n"),
    io:format("  c1 -> proc_add10~n"),
    io:format("  ... etc with proper context separation~n~n"),
    
    %% Connect both clients
    ok = gen_server:call(client1, connect),
    ok = gen_server:call(client2, connect),
    
    StartTime = erlang:system_time(millisecond),
    
    %% Send multiple concurrent requests to create interleaving
    Pid1 = spawn(fun() ->
        io:format("[CLIENT] Client1 starting request 5...~n"),
        Result1 = gen_server:call(client1, {send_number, 5}),
        io:format("[CLIENT] Client1 got result: ~p (expected: 30)~n", [Result1])
    end),
    
    %% Small delay to ensure client1 starts first but client2 might finish first
    timer:sleep(50),
    
    Pid2 = spawn(fun() ->
        io:format("[CLIENT] Client2 starting request 2...~n"),
        Result2 = gen_server:call(client2, {send_number, 2}),
        io:format("[CLIENT] Client2 got result: ~p (expected: 24)~n", [Result2])
    end),
    
    %% Another request from client1 after a short delay
    timer:sleep(100),
    
    Pid3 = spawn(fun() ->
        io:format("[CLIENT] Client1 starting second request 8...~n"),
        Result3 = gen_server:call(client1, {send_number, 8}),
        io:format("[CLIENT] Client1 second result: ~p (expected: 36)~n", [Result3])
    end),
    
    %% And one more from client2
    timer:sleep(75),
    
    Pid4 = spawn(fun() ->
        io:format("[CLIENT] Client2 starting second request 15...~n"),
        Result4 = gen_server:call(client2, {send_number, 15}),
        io:format("[CLIENT] Client2 second result: ~p (expected: 50)~n", [Result4])
    end),
    
    %% Wait for all client requests to complete
    wait_for_pids([Pid1, Pid2, Pid3, Pid4]),
    
    EndTime = erlang:system_time(millisecond),
    TotalTime = EndTime - StartTime,
    
    io:format("~nAll async requests completed in ~p ms~n", [TotalTime]),
    io:format("Check the monitor output above to verify:~n"),
    io:format("  1. Different contexts for each request~n"),
    io:format("  2. Interleaved processing (not strictly sequential)~n"),
    io:format("  3. Proper context preservation throughout each chain~n").

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
    io:format("Cleaning up async system...~n"),
    
    %% Disconnect clients first
    catch gen_server:call(client1, disconnect),
    catch gen_server:call(client2, disconnect),
    
    %% Stop monitor
    catch monitor:stop(),
    
    %% Stop concurrency logger
    catch concurrency_logger:stop(),
    
    %% Stop clients
    stop_process(client1),
    stop_process(client2),
    
    %% Stop server components
    stop_process(slow_central_server),
    stop_process(slow_proc_add10),
    stop_process(slow_proc_mul2),
    
    %% Stop simple_wrapper
    stop_process(simple_wrapper),
    
    io:format("Async cleanup complete.~n").

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