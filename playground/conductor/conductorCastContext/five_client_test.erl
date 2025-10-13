-module(five_client_test).
-export([run_five_client_test/0, cleanup/0]).

%% Test with 5 clients each making one request
run_five_client_test() ->
    io:format("=== Five Client Concurrency Test ===~n"),
    io:format("Testing 5 clients each making one concurrent request~n"),
    
    %% Start all system components
    setup_system(),
    
    %% Run the 5-client test
    io:format("~n=== Starting 5 Concurrent Requests ===~n"),
    test_five_concurrent_requests(),
    
    timer:sleep(3000), % Wait for all async operations to complete
    
    %% Cleanup
    cleanup(),
    
    io:format("~n=== Five Client Concurrency Test Complete ===~n").

%% Setup all system components
setup_system() ->
    io:format("Setting up system for 5-client test...~n"),
    
    %% Start concurrency logger first
    {ok, _} = concurrency_logger:start(),
    
    %% Start the simple_wrapper first (required for context injection)
    {ok, _} = simple_wrapper:start_link(),
    
    %% Start the processing components
    {ok, _} = slow_central_server:start_link(),
    
    %% Start monitor for context verification
    {ok, _} = monitor:start(),
    
    %% Start 5 clients
    ClientNames = [client1, client2, client3, client4, client5],
    lists:foreach(fun(ClientName) ->
        {ok, _} = client:start_link(ClientName),
        %% Connect each client
        ok = gen_server:call(ClientName, connect)
    end, ClientNames),
    
    timer:sleep(100), % Give processes time to start
    io:format("System setup complete - 5 clients ready.~n").

%% Test 5 concurrent requests
test_five_concurrent_requests() ->
    io:format("Launching 5 concurrent requests with different numbers...~n"),
    io:format("Expected pattern: All requests start nearly simultaneously,~n"),
    io:format("process in parallel through the pipeline with proper context isolation~n~n"),
    
    StartTime = erlang:system_time(millisecond),
    
    %% Define client configurations: {ClientName, Number, DelayMs}
    ClientConfigs = [
        {client1, 10, 0},    % 10 -> 20 -> 40 (immediate)
        {client2, 3, 25},    % 3 -> 13 -> 26 (25ms delay)  
        {client3, 7, 50},    % 7 -> 17 -> 34 (50ms delay)
        {client4, 15, 75},   % 15 -> 25 -> 50 (75ms delay)
        {client5, 2, 100}    % 2 -> 12 -> 24 (100ms delay)
    ],
    
    %% Spawn all client workers simultaneously
    Workers = lists:map(fun({ClientName, Number, Delay}) ->
        spawn(fun() ->
            timer:sleep(Delay),
            ReqStartTime = erlang:system_time(millisecond),
            RelativeStart = ReqStartTime - StartTime,
            io:format("[~p ms] [~p_WORKER] STARTING request: number=~p (delay=~pms)~n", 
                     [ReqStartTime, ClientName, Number, Delay]),
            
            try
                Result = gen_server:call(ClientName, {send_number, Number}),
                
                ReqEndTime = erlang:system_time(millisecond),
                Duration = ReqEndTime - ReqStartTime,
                RelativeEnd = ReqEndTime - StartTime,
                io:format("[~p ms] [~p_WORKER] FINISHED: Result=~p, Duration=~pms (relative: ~pms)~n", 
                         [ReqEndTime, ClientName, Result, Duration, RelativeEnd])
            catch
                Error:Reason ->
                    io:format("[~p_WORKER] ERROR: ~p:~p~n", [ClientName, Error, Reason])
            end
        end)
    end, ClientConfigs),
    
    %% Wait for all workers to complete
    wait_for_workers(Workers),
    
    EndTime = erlang:system_time(millisecond),
    TotalTestTime = EndTime - StartTime,
    
    io:format("~n=== FIVE CLIENT CONCURRENCY ANALYSIS ===~n"),
    io:format("[TEST] Total test time: ~pms~n", [TotalTestTime]),
    %io:format("[TEST] Expected if SEQUENTIAL: ~1250ms (5 × ~250ms each)~n"),
    %io:format("[TEST] Expected if CONCURRENT: ~400-500ms (parallel + stagger)~n"),
    
    if
        TotalTestTime < 600 ->
            io:format("[TEST] ✓ STRONG CONCURRENCY: Time strongly indicates parallel processing!~n");
        TotalTestTime < 800 ->
            io:format("[TEST] ✓ GOOD CONCURRENCY: Time suggests mostly parallel processing~n");
        TotalTestTime >= 800 ->
            io:format("[TEST] ✗ POSSIBLE SERIALIZATION: Time suggests sequential processing~n")
    end,
    
    io:format("~n[TEST] === CONCURRENCY INDICATORS TO CHECK ===~n"),
    io:format("[TEST] Look for these patterns in the logs above:~n"),
    io:format("[TEST] 1. Multiple worker PIDs active simultaneously~n"),
    io:format("[TEST] 2. Overlapping timestamps across different contexts~n"),
    io:format("[TEST] 3. Requests completing OUT OF ORDER (not 1,2,3,4,5)~n"),
    io:format("[TEST] 4. Multiple 'SPAWNED async worker' messages close together~n"),
    io:format("[TEST] 5. Context isolation - each request maintains unique context~n~n").

%% Wait for all worker processes to complete
wait_for_workers([]) ->
    ok;
wait_for_workers([Pid | Rest]) ->
    case is_process_alive(Pid) of
        true ->
            timer:sleep(10),
            wait_for_workers([Pid | Rest]);
        false ->
            wait_for_workers(Rest)
    end.

%% Cleanup all processes
cleanup() ->
    io:format("Cleaning up 5-client system...~n"),
    
    %% Disconnect all clients
    ClientNames = [client1, client2, client3, client4, client5],
    lists:foreach(fun(ClientName) ->
        catch gen_server:call(ClientName, disconnect)
    end, ClientNames),
    
    %% Stop monitor
    catch monitor:stop(),
    
    %% Stop concurrency logger
    catch concurrency_logger:stop(),
    
    %% Stop clients
    lists:foreach(fun(ClientName) ->
        stop_process(ClientName)
    end, ClientNames),
    
    %% Stop server components
    stop_process(slow_central_server),
    stop_process(slow_proc_add10),
    stop_process(slow_proc_mul2),
    
    %% Stop simple_wrapper
    stop_process(simple_wrapper),
    
    io:format("5-client cleanup complete.~n").

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