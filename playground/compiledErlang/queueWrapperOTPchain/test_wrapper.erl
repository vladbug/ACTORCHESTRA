-module(test_wrapper).
-export([run_test/0, run_stress_test/0]).

%% Test configuration
-define(NUM_CLIENTS, 5).
-define(REQUESTS_PER_CLIENT, 10).
-define(WORKER_DELAY_MS, 100).  % Simulate some processing time

%% Main test function
run_test() ->
    io:format("=== Starting Wrapper Test ===~n"),
    
    %% Start the wrapper and worker
    {ok, _} = wrapper:start_link(),
    {ok, _} = test_worker:start_link(),
    
    io:format("Started wrapper and worker~n"),
    
    %% Start multiple clients
    _ = [test_client:start_client(I, ?REQUESTS_PER_CLIENT) || I <- lists:seq(1, ?NUM_CLIENTS)],
    
    io:format("Started ~p clients, each making ~p requests~n", [?NUM_CLIENTS, ?REQUESTS_PER_CLIENT]),
    
    %% Wait for all clients to complete
    timer:sleep((?NUM_CLIENTS * ?REQUESTS_PER_CLIENT * ?WORKER_DELAY_MS) + 5000),
    
    io:format("=== Test Complete ===~n"),
    ok.

%% More aggressive stress test
run_stress_test() ->
    io:format("=== Starting Stress Test ===~n"),
    
    %% Start the wrapper and worker  
    {ok, _} = wrapper:start_link(),
    {ok, _} = test_worker:start_link(),
    
    %% Create a burst of concurrent requests
    NumClients = 20,
    RequestsPerClient = 5,
    
    io:format("Starting ~p clients with ~p requests each~n", [NumClients, RequestsPerClient]),
    
    %% Start all clients simultaneously
    _ = [test_client:start_client(I, RequestsPerClient) || I <- lists:seq(1, NumClients)],
    
    %% Wait for completion
    timer:sleep((NumClients * RequestsPerClient * ?WORKER_DELAY_MS div 2) + 10000),
    
    io:format("=== Stress Test Complete ===~n"),
    ok.
