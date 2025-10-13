%% stress_test.erl
-module(stress_test).
-export([run_burst/0, run_with_delays/0, run_simultaneous/0]).

%% Test with many clients sending requests simultaneously
run_burst() ->
    io:format("=== BURST TEST: Multiple clients sending simultaneously ===~n"),
    
    %% Start multiple clients that all send at the same time
    NumClients = 5,
    RequestsPerClient = 3,
    
    StartTime = erlang:system_time(millisecond),
    
    %% Spawn all clients at once
    ClientPids = lists:map(fun(ClientNum) ->
        spawn(fun() -> burst_client_runner(ClientNum, RequestsPerClient) end)
    end, lists:seq(1, NumClients)),
    
    %% Wait for all to finish
    lists:foreach(fun(Pid) ->
        monitor(process, Pid),
        receive
            {'DOWN', _, process, _Pid, _} -> ok
        after 10000 ->
            io:format("Timeout waiting for client ~p~n", [Pid])
        end
    end, ClientPids),
    
    EndTime = erlang:system_time(millisecond),
    TotalTime = EndTime - StartTime,
    TotalRequests = NumClients * RequestsPerClient,
    
    io:format("=== BURST RESULTS ===~n"),
    io:format("Total time: ~p ms~n", [TotalTime]),
    io:format("Total requests: ~p~n", [TotalRequests]),
    io:format("Average time per request: ~.2f ms~n", [TotalTime / TotalRequests]),
    io:format("Requests per second: ~.2f~n", [TotalRequests * 1000 / TotalTime]).

%% Add artificial delays to make concurrency more visible
run_with_delays() ->
    io:format("=== DELAYED TEST: Adding delays to show concurrency ===~n"),
    
    %% Temporarily modify the processing to add delays
    %% We'll simulate this by having clients send overlapping requests
    
    spawn(fun() -> delayed_client_runner(1, [1, 2, 3], 0) end),
    spawn(fun() -> delayed_client_runner(2, [10, 11, 12], 50) end),
    spawn(fun() -> delayed_client_runner(3, [20, 21, 22], 100) end),
    spawn(fun() -> delayed_client_runner(4, [30, 31, 32], 150) end),
    
    timer:sleep(5000),
    io:format("=== DELAYED TEST COMPLETE ===~n").

%% Send multiple requests from same client without waiting
run_simultaneous() ->
    io:format("=== SIMULTANEOUS TEST: One client sending multiple requests ===~n"),
    
    %% Start one client
    {ok, _} = client:start_link(test_client),
    ok = client:connect(test_client),
    
    %% Send multiple requests without waiting for responses
    Requests = lists:seq(1, 5),
    StartTime = erlang:system_time(millisecond),
    
    %% Spawn a process for each request to send them truly simultaneously
    RequestPids = lists:map(fun(Num) ->
        spawn(fun() ->
            ReqStart = erlang:system_time(millisecond),
            Result = client:send_number(test_client, Num),
            ReqEnd = erlang:system_time(millisecond),
            io:format("SIMULTANEOUS: Request ~p completed in ~p ms with result ~p~n", 
                     [Num, ReqEnd - ReqStart, Result])
        end)
    end, Requests),
    
    %% Wait for all requests to complete
    lists:foreach(fun(Pid) ->
        monitor(process, Pid),
        receive
            {'DOWN', _, process, Pid, _} -> ok
        after 5000 ->
            io:format("Timeout on request~n")
        end
    end, RequestPids),
    
    EndTime = erlang:system_time(millisecond),
    io:format("All simultaneous requests completed in ~p ms total~n", [EndTime - StartTime]),
    
    client:disconnect(test_client).

%% Helper functions
burst_client_runner(ClientNum, NumRequests) ->
    ClientName = list_to_atom("burst_client" ++ integer_to_list(ClientNum)),
    {ok, _} = client:start_link(ClientName),
    ok = client:connect(ClientName),
    
    StartTime = erlang:system_time(millisecond),
    
    %% Send all requests rapidly
    Results = lists:map(fun(N) ->
        RequestStart = erlang:system_time(millisecond),
        Input = ClientNum * 100 + N,  % Unique numbers per client
        Result = client:send_number(ClientName, Input),
        RequestEnd = erlang:system_time(millisecond),
        {Input, Result, RequestEnd - RequestStart}
    end, lists:seq(1, NumRequests)),
    
    EndTime = erlang:system_time(millisecond),
    TotalTime = EndTime - StartTime,
    
    io:format("Client~p finished ~p requests in ~p ms: ~p~n", 
             [ClientNum, NumRequests, TotalTime, Results]),
    
    client:disconnect(ClientName).

delayed_client_runner(ClientNum, Numbers, InitialDelay) ->
    timer:sleep(InitialDelay),
    
    ClientName = list_to_atom("delayed_client" ++ integer_to_list(ClientNum)),
    {ok, _} = client:start_link(ClientName),
    ok = client:connect(ClientName),
    
    lists:foreach(fun(N) ->
        RequestStart = erlang:system_time(millisecond),
        io:format("[~p ms] Client~p SENDING ~p~n", [RequestStart, ClientNum, N]),
        
        %% Send request in a spawned process so we can see overlapping
        spawn(fun() ->
            CallStart = erlang:system_time(millisecond),
            Result = client:send_number(ClientName, N),
            CallEnd = erlang:system_time(millisecond),
            io:format("[~p ms] Client~p RECEIVED ~p -> ~p (took ~p ms)~n", 
                     [CallEnd, ClientNum, N, Result, CallEnd - CallStart])
        end),
        
        %% Small delay between requests from same client
        timer:sleep(200)
    end, Numbers),
    
    %% Wait a bit before disconnecting to let spawned requests finish
    timer:sleep(1000),
    client:disconnect(ClientName),
    io:format("Client~p disconnected~n", [ClientNum]).
