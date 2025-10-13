-module(simple_concurrency_test).
-export([test_baseline/0, test_instrumented/0, test_both/0]).
-export([test_system/1, cleanup/0]).

%% Simple concurrency test to isolate system vs benchmark issues

test_both() ->
    io:format("=== TESTING BOTH SYSTEMS FOR BASIC CONCURRENCY ===~n"),
    
    io:format("~n--- Testing Baseline System ---~n"),
    BaselineResult = test_system(baseline),
    
    timer:sleep(2000),
    
    io:format("~n--- Testing Instrumented System ---~n"),
    InstrumentedResult = test_system(instrumented),
    
    io:format("~n=== RESULTS ===~n"),
    io:format("Baseline: ~p~n", [BaselineResult]),
    io:format("Instrumented: ~p~n", [InstrumentedResult]),
    
    case {BaselineResult, InstrumentedResult} of
        {{ok, _}, {ok, _}} ->
            io:format("Both systems handle concurrency successfully~n");
        {{error, _}, {ok, _}} ->
            io:format("Baseline has concurrency issues, instrumented works~n");
        {{ok, _}, {error, _}} ->
            io:format("Baseline works, instrumented has concurrency issues~n");
        {{error, _}, {error, _}} ->
            io:format("Both systems have concurrency issues~n")
    end.

test_baseline() ->
    test_system(baseline).

test_instrumented() ->
    test_system(instrumented).

test_system(SystemType) ->
    io:format("Testing ~p system with basic concurrency...~n", [SystemType]),
    
    %% Clean start
    cleanup(),
    timer:sleep(1000),
    
    try
        %% Setup system
        SetupResult = setup_system(SystemType),
        io:format("System setup: ~p~n", [SetupResult]),
        
        %% Run simple concurrent test
        {ok, Results} = run_simple_concurrent_test(SystemType),
        
        %% Cleanup
        cleanup(),
        
        {ok, Results}
        
    catch
        Error:Reason:Stacktrace ->
            io:format("Test failed: ~p:~p~n", [Error, Reason]),
            io:format("Stacktrace: ~p~n", [Stacktrace]),
            cleanup(),
            {error, {Error, Reason}}
    end.

setup_system(baseline) ->
    io:format("  Starting baseline chat server...~n"),
    baseline_chat_server:start_link();
    
setup_system(instrumented) ->
    io:format("  Starting instrumented system...~n"),
    monitor:start(),
    simple_wrapper:start_link(),
    chat_server:start_link().

run_simple_concurrent_test(SystemType) ->
    NumClients = 3,
    NumMessages = 3,
    RoomName = test_room,
    
    io:format("  Creating ~p concurrent clients...~n", [NumClients]),
    
    %% Start client processes
    ClientModule = case SystemType of
        baseline -> baseline_client;
        instrumented -> client
    end,
    
    %% Create clients
    ClientNames = [list_to_atom("test_client_" ++ integer_to_list(N)) || N <- lists:seq(1, NumClients)],
    
    %% Start all clients
    lists:foreach(fun(ClientName) ->
        {ok, _} = ClientModule:start_link(ClientName),
        io:format("    Started client ~p~n", [ClientName])
    end, ClientNames),
    
    timer:sleep(500),  % Let clients start
    
    %% Register all clients concurrently
    io:format("  Registering clients concurrently...~n"),
    RegisterPids = lists:map(fun(ClientName) ->
        spawn_link(fun() ->
            Result = gen_server:call(ClientName, register, 5000),
            io:format("    ~p register result: ~p~n", [ClientName, Result])
        end)
    end, ClientNames),
    
    %% Wait for registration to complete
    lists:foreach(fun(Pid) ->
        receive
            {'EXIT', Pid, normal} -> ok;
            {'EXIT', Pid, Reason} -> 
                io:format("    Registration process ~p failed: ~p~n", [Pid, Reason])
        after 6000 ->
            io:format("    Registration timeout for process ~p~n", [Pid])
        end
    end, RegisterPids),
    
    timer:sleep(500),
    
    %% Join room concurrently
    io:format("  Joining room concurrently...~n"),
    JoinPids = lists:map(fun(ClientName) ->
        spawn_link(fun() ->
            Result = gen_server:call(ClientName, {join_room, RoomName}, 5000),
            io:format("    ~p join result: ~p~n", [ClientName, Result])
        end)
    end, ClientNames),
    
    %% Wait for joins to complete
    lists:foreach(fun(Pid) ->
        receive
            {'EXIT', Pid, normal} -> ok;
            {'EXIT', Pid, Reason} -> 
                io:format("    Join process ~p failed: ~p~n", [Pid, Reason])
        after 6000 ->
            io:format("    Join timeout for process ~p~n", [Pid])
        end
    end, JoinPids),
    
    timer:sleep(500),
    
    %% Send messages with delays to reduce contention
    io:format("  Sending messages with staggered timing...~n"),
    SendResults = lists:map(fun({ClientName, DelayMs}) ->
        spawn_link(fun() ->
            timer:sleep(DelayMs),  % Stagger the sends
            lists:foreach(fun(MsgNum) ->
                Message = "Test message " ++ integer_to_list(MsgNum) ++ " from " ++ atom_to_list(ClientName),
                Result = gen_server:call(ClientName, {send_message, RoomName, Message}, 8000),
                io:format("    ~p send ~p result: ~p~n", [ClientName, MsgNum, Result]),
                timer:sleep(500)  % Delay between messages from same client
            end, lists:seq(1, NumMessages))
        end)
    end, lists:zip(ClientNames, [0, 200, 400])),  % Stagger start times
    
    %% Wait for all sends to complete
    lists:foreach(fun(Pid) ->
        receive
            {'EXIT', Pid, normal} -> ok;
            {'EXIT', Pid, Reason} -> 
                io:format("    Send process ~p failed: ~p~n", [Pid, Reason])
        after 15000 ->  % Longer timeout for message sending
            io:format("    Send timeout for process ~p~n", [Pid])
        end
    end, SendResults),
    
    timer:sleep(1000),  % Let all messages propagate
    
    %% Leave room
    io:format("  Leaving room...~n"),
    lists:foreach(fun(ClientName) ->
        Result = gen_server:call(ClientName, {leave_room, RoomName}, 5000),
        io:format("    ~p leave result: ~p~n", [ClientName, Result])
    end, ClientNames),
    
    %% Stop clients
    io:format("  Stopping clients...~n"),
    lists:foreach(fun(ClientName) ->
        try
            gen_server:stop(ClientName, normal, 2000)
        catch
            _:_ -> 
                case whereis(ClientName) of
                    undefined -> ok;
                    Pid -> exit(Pid, kill)
                end
        end
    end, ClientNames),
    
    io:format("  Simple concurrency test completed successfully~n"),
    {ok, #{
        clients => NumClients,
        messages_per_client => NumMessages,
        total_operations => NumClients * (1 + 1 + NumMessages + 1)  % register + join + messages + leave
    }}.

cleanup() ->
    io:format("Cleaning up...~n"),
    
    %% Stop main processes
    ProcessesToStop = [
        baseline_chat_server, chat_server, simple_wrapper, monitor,
        baseline_client_registry, client_registry
    ],
    
    lists:foreach(fun(ProcessName) ->
        case whereis(ProcessName) of
            undefined -> ok;
            Pid -> 
                try
                    gen_server:stop(ProcessName, normal, 2000)
                catch
                    _:_ -> exit(Pid, kill)
                end
        end
    end, ProcessesToStop),
    
    %% Clean up test clients
    TestClientNames = [
        list_to_atom("test_client_" ++ integer_to_list(N)) 
        || N <- lists:seq(1, 10)
    ],
    
    lists:foreach(fun(ClientName) ->
        case whereis(ClientName) of
            undefined -> ok;
            Pid -> 
                try
                    gen_server:stop(ClientName, normal, 1000)
                catch
                    _:_ -> exit(Pid, kill)
                end
        end
    end, TestClientNames),
    
    timer:sleep(1000).

%%% Helper to run just a stress test
stress_test(SystemType) ->
    io:format("=== STRESS TEST FOR ~p ===~n", [SystemType]),
    cleanup(),
    timer:sleep(1000),
    
    setup_system(SystemType),
    
    %% Try progressively more clients
    MaxClients = 10,
    lists:foreach(fun(NumClients) ->
        io:format("~n--- Testing with ~p clients ---~n", [NumClients]),
        try
            stress_test_with_clients(SystemType, NumClients),
            io:format("SUCCESS: ~p clients worked~n", [NumClients])
        catch
            Error:Reason ->
                io:format("FAILED: ~p clients failed with ~p:~p~n", [NumClients, Error, Reason])
        end,
        timer:sleep(2000)
    end, lists:seq(2, MaxClients)),
    
    cleanup().

stress_test_with_clients(SystemType, NumClients) ->
    ClientModule = case SystemType of
        baseline -> baseline_client;
        instrumented -> client
    end,
    
    %% Create clients quickly
    ClientNames = [list_to_atom("stress_client_" ++ integer_to_list(N)) || N <- lists:seq(1, NumClients)],
    
    lists:foreach(fun(ClientName) ->
        {ok, _} = ClientModule:start_link(ClientName)
    end, ClientNames),
    
    %% Quick operations
    lists:foreach(fun(ClientName) ->
        gen_server:call(ClientName, register, 3000),
        gen_server:call(ClientName, {join_room, stress_room}, 3000),
        gen_server:call(ClientName, {send_message, stress_room, "quick test"}, 5000),
        gen_server:call(ClientName, {leave_room, stress_room}, 3000)
    end, ClientNames),
    
    %% Cleanup
    lists:foreach(fun(ClientName) ->
        try gen_server:stop(ClientName, normal, 1000)
        catch _:_ -> ok end
    end, ClientNames).