-module(simple_concurrent_test).
-export([run/0]).

%% Simple demonstration of concurrent system performance
run() ->
    io:format("~n=== CONCURRENT SYSTEM DEMONSTRATION ===~n"),
    io:format("Testing with 1, 2, 4, and 8 clients...~n~n"),
    
    %% Test configurations
    Configs = [
        {1, 5},   % 1 client, 5 operations
        {2, 5},   % 2 clients, 5 operations each
        {4, 2},   % 4 clients, 2 operations each
        {8, 2}    % 8 clients, 2 operations each
    ],
    
    %% Run each test
    lists:foreach(fun({NumClients, OpsPerClient}) ->
        run_test(NumClients, OpsPerClient),
        timer:sleep(500),  % Pause between tests
        io:format("~n")
    end, Configs),
    
    io:format("=== DEMONSTRATION COMPLETE ===~n").

%% Run a single concurrent test
run_test(NumClients, OpsPerClient) ->
    io:format("--- Testing with ~p client(s), ~p operations each ---~n", 
              [NumClients, OpsPerClient]),
    
    %% Setup server
    {ok, _} = slow_central_server:start_link(),
    
    %% Spawn clients
    Coordinator = self(),
    ClientPids = [spawn_link(fun() -> 
        client_worker(Id, OpsPerClient, Coordinator) 
    end) || Id <- lists:seq(1, NumClients)],
    
    %% Start timing and execute
    StartTime = erlang:system_time(microsecond),
    lists:foreach(fun(Pid) -> Pid ! start end, ClientPids),
    
    %% Wait for all clients to finish
    wait_for_clients(NumClients),
    
    EndTime = erlang:system_time(microsecond),
    ElapsedMs = (EndTime - StartTime) / 1000,
    
    %% Show results
    TotalOps = NumClients * OpsPerClient,
    Throughput = TotalOps / (ElapsedMs / 1000),
    
    io:format("  Total operations: ~p~n", [TotalOps]),
    io:format("  Time: ~.2f ms~n", [ElapsedMs]),
    io:format("  Throughput: ~.2f ops/sec~n", [Throughput]),
    
    %% Cleanup
    cleanup().

%% Simple client worker
client_worker(ClientId, NumOps, Coordinator) ->
    ClientName = list_to_atom("client_" ++ integer_to_list(ClientId)),
    {ok, _} = client:start_link(ClientName),
    ok = gen_server:call(ClientName, connect),
    
    receive start -> ok end,
    
    %% Execute operations
    lists:foreach(fun(N) ->
        gen_server:call(ClientName, {send_number, N rem 50}, 10000)
    end, lists:seq(1, NumOps)),
    
    %% Cleanup and signal done
    gen_server:call(ClientName, disconnect),
    gen_server:stop(ClientName),
    Coordinator ! done.

%% Wait for all clients to complete
wait_for_clients(0) -> ok;
wait_for_clients(N) ->
    receive
        done -> wait_for_clients(N - 1)
    after 30000 ->
        error(timeout)
    end.

%% Cleanup processes
cleanup() ->
    Names = [slow_central_server, slow_proc_add10, slow_proc_mul2],
    lists:foreach(fun(Name) ->
        case whereis(Name) of
            undefined -> ok;
            Pid -> 
                try gen_server:stop(Name) 
                catch _:_ -> exit(Pid, kill) 
                end
        end
    end, Names),
    timer:sleep(100).