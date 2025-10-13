-module(concurrency_test).
-export([verify_concurrency/0, setup_system/0, cleanup/0]).

%% Test specifically designed to prove concurrent processing with 5 clients
verify_concurrency() ->
    io:format("=== 5-CLIENT CONCURRENCY VERIFICATION TEST ===~n"),
    io:format("This test will prove whether concurrent processing actually happens~n"),
    io:format("Look for OVERLAPPING timestamps and OUT-OF-ORDER completion!~n~n"),
    
    %% Setup system
    setup_system(),
    
    %% Test with 5 clients sending different numbers
    TestStartTime = erlang:system_time(millisecond),
    io:format("[~p ms] [TEST] Starting 5-client concurrency verification...~n", [TestStartTime]),
    
    %% Start 5 clients with staggered timing (25ms apart) and different numbers
    ClientConfigs = [
        {client1, 15, 20},     % 15 -> 25 -> 50 (immediate start)
        {client2, 8, 0},     % 8 -> 18 -> 36 (25ms delay)
        {client3, 22, 0},    % 22 -> 32 -> 64 (50ms delay)  
        {client4, 5, 75},     % 5 -> 15 -> 30 (75ms delay)
        {client5, 12, 100}    % 12 -> 22 -> 44 (100ms delay)
    ],
    
    %% Spawn all client workers
    Workers = lists:map(fun({ClientName, Number, Delay}) ->
        spawn(fun() ->
            timer:sleep(Delay),
            StartTime = erlang:system_time(millisecond),
            RelativeTime = StartTime - TestStartTime,
            io:format("[~p ms] [~p_WORKER] STARTING: number=~p (delay was ~p ms)~n", 
                     [StartTime, ClientName, Number, Delay]),
            
            try
                Result = simple_client:send_number(ClientName, Number),
                
                EndTime = erlang:system_time(millisecond),
                Duration = EndTime - StartTime,
                RelativeEnd = EndTime - TestStartTime,
                io:format("[~p ms] [~p_WORKER] FINISHED: Result=~p, Duration=~p ms (relative: ~p ms)~n", 
                         [EndTime, ClientName, Result, Duration, RelativeEnd])
            catch
                Error:Reason ->
                    io:format("[~p_WORKER] ERROR: ~p:~p~n", [ClientName, Error, Reason])
            end
        end)
    end, ClientConfigs),
    
    %% Wait for all to complete
    wait_for_completion(Workers),
    
    TestEndTime = erlang:system_time(millisecond),
    TotalTestTime = TestEndTime - TestStartTime,
    
    io:format("~n[~p ms] [TEST] === 5-CLIENT CONCURRENCY ANALYSIS ===~n", [TestEndTime]),
    io:format("[TEST] Total test time: ~p ms~n", [TotalTestTime]),
    io:format("[TEST] Expected if SEQUENTIAL: ~~1350ms (5 * ~~270ms average)~n", []),
    io:format("[TEST] Expected if CONCURRENT: ~~400-500ms (overlapping + stagger)~n", []),
    
    if
        TotalTestTime < 800 ->
            io:format("[TEST] ✓ STRONG CONCURRENCY: Time strongly suggests concurrent processing!~n");
        TotalTestTime < 1000 ->
            io:format("[TEST] ✓ LIKELY CONCURRENCY: Time suggests mostly concurrent processing~n");
        TotalTestTime >= 1000 ->
            io:format("[TEST] ✗ POSSIBLE SERIALIZATION: Time suggests sequential processing~n")
    end,
    
    io:format("~n[TEST] === MANUAL VERIFICATION CHECKLIST ===~n"),
    io:format("[TEST] Look for these concurrency indicators in the logs above:~n"),
    io:format("[TEST] 1. Multiple ADD_SERVER_WORKER processes active simultaneously~n"),
    io:format("[TEST] 2. MUL_SERVER receiving requests while others still in ADD_SERVER~n"),
    io:format("[TEST] 3. Client workers finishing OUT OF ORDER (not 1,2,3,4,5)~n"),
    io:format("[TEST] 4. Overlapping timestamps across different client processing~n"),
    io:format("[TEST] 5. ADD_SERVER 'ready for next request' messages close together~n~n"),
    
    %% Cleanup
    cleanup(),
    
    io:format("[TEST] 5-client concurrency verification complete.~n").

%% Worker for Client1 (processes number 10)
client1_worker(StartTime) ->
    ProcessStartTime = erlang:system_time(millisecond),
    io:format("[~p ms] [CLIENT1_WORKER] Starting request (10 -> +10 -> 20 -> *2 -> 40)~n", 
             [ProcessStartTime]),
    
    try
        Result = simple_client:send_number(client1, 10),
        
        EndTime = erlang:system_time(millisecond),
        Duration = EndTime - StartTime,
        io:format("[~p ms] [CLIENT1_WORKER] FINISHED: Result=~p, Duration=~p ms~n", 
                 [EndTime, Result, Duration])
    catch
        Error:Reason ->
            io:format("[CLIENT1_WORKER] ERROR: ~p:~p~n", [Error, Reason])
    end.

%% Worker for Client2 (processes number 3) 
client2_worker(StartTime) ->
    ProcessStartTime = erlang:system_time(millisecond),
    io:format("[~p ms] [CLIENT2_WORKER] Starting request (3 -> +10 -> 13 -> *2 -> 26)~n", 
             [ProcessStartTime]),
    
    try
        Result = simple_client:send_number(client2, 3),
        
        EndTime = erlang:system_time(millisecond),
        Duration = EndTime - StartTime,
        io:format("[~p ms] [CLIENT2_WORKER] FINISHED: Result=~p, Duration=~p ms~n", 
                 [EndTime, Result, Duration])
    catch
        Error:Reason ->
            io:format("[CLIENT2_WORKER] ERROR: ~p:~p~n", [Error, Reason])
    end.

%% Wait for all worker processes to complete
wait_for_completion([]) ->
    ok;
wait_for_completion([Pid | Rest]) ->
    case is_process_alive(Pid) of
        true ->
            timer:sleep(10),
            wait_for_completion([Pid | Rest]);
        false ->
            wait_for_completion(Rest)
    end.

%% Setup all system components  
setup_system() ->
    io:format("[SETUP] Starting system components...~n"),
    
    %% Start wrapper
    case simple_wrapper:start_link() of
        {ok, _} -> io:format("[SETUP] ✓ Wrapper started~n");
        {error, {already_started, _}} -> io:format("[SETUP] ✓ Wrapper already running~n")
    end,
    
    %% Start servers
    case add_server:start_link() of
        {ok, _} -> io:format("[SETUP] ✓ Add server started~n");
        {error, {already_started, _}} -> io:format("[SETUP] ✓ Add server already running~n")
    end,
    
    case mul_server:start_link() of
        {ok, _} -> io:format("[SETUP] ✓ Mul server started~n");
        {error, {already_started, _}} -> io:format("[SETUP] ✓ Mul server already running~n")
    end,
    
    %% Start monitor
    case simple_monitor:start() of
        {ok, _} -> io:format("[SETUP] ✓ Monitor started~n");
        {ok, _} -> io:format("[SETUP] ✓ Monitor already running~n")
    end,
    
    %% Start 5 clients
    ClientNames = [client1, client2, client3, client4, client5],
    lists:foreach(fun(ClientName) ->
        case simple_client:start_link(ClientName) of
            {ok, _} -> io:format("[SETUP] ✓ ~p started~n", [ClientName]);
            {error, {already_started, _}} -> io:format("[SETUP] ✓ ~p already running~n", [ClientName])
        end
    end, ClientNames),
    
    timer:sleep(100), % Allow processes to initialize
    io:format("[SETUP] System ready for 5-client concurrency test.~n~n").

%% Cleanup all processes
cleanup() ->
    io:format("~n[CLEANUP] Stopping system components...~n"),
    
    %% Stop monitor
    catch simple_monitor:stop(),
    
    %% Stop all 5 clients  
    ClientNames = [client1, client2, client3, client4, client5],
    lists:foreach(fun(ClientName) ->
        stop_process(ClientName)
    end, ClientNames),
    
    %% Stop servers
    stop_process(add_server),
    stop_process(mul_server),
    
    %% Stop wrapper
    stop_process(simple_wrapper),
    
    io:format("[CLEANUP] Complete.~n").

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

%% Wait for all worker processes to complete

%% Additional test: Stress test with multiple concurrent requests
stress_test_concurrency() ->
    io:format("=== STRESS TEST: 5 CONCURRENT REQUESTS ===~n"),
    setup_system(),
    
    StartTime = erlang:system_time(millisecond),
    
    %% Spawn 5 concurrent requests
    Workers = [spawn(fun() -> 
        ReqStart = erlang:system_time(millisecond),
        io:format("[~p ms] [STRESS_WORKER_~p] Starting...~n", [ReqStart, N]),
        Result = simple_client:send_number(client1, N),
        ReqEnd = erlang:system_time(millisecond),
        io:format("[~p ms] [STRESS_WORKER_~p] Finished: ~p~n", [ReqEnd, N, Result])
    end) || N <- [1,2,3,4,5]],
    
    wait_for_completion(Workers),
    
    EndTime = erlang:system_time(millisecond),
    TotalTime = EndTime - StartTime,
    
    io:format("Stress test completed in ~p ms~n", [TotalTime]),
    io:format("Sequential would take ~1250ms (5 * 250ms)~n"),
    io:format("Concurrent should take ~250-300ms~n"),
    
    cleanup().