-module(simple_chat_test).
-export([run/0]).

%% Simple demonstration of chat system
run() ->
    io:format("~n=== CHAT SYSTEM DEMONSTRATION ===~n"),
    io:format("Testing with 1, 2, and 4 clients...~n~n"),
    
    %% Test configurations: {clients, messages per client}
    Configs = [{1, 5}, {2, 5}, {4, 5}],
    
    lists:foreach(fun({NumClients, Messages}) ->
        run_session(NumClients, Messages),
        cleanup(),
        timer:sleep(300),
        io:format("~n")
    end, Configs),
    
    io:format("=== DEMONSTRATION COMPLETE ===~n").

%% Run a chat session
run_session(NumClients, MessagesPerClient) ->
    io:format("--- Testing ~p client(s), ~p messages each ---~n", 
              [NumClients, MessagesPerClient]),
    RoomName = test_room,
    
    %% Setup chat server
    {ok, _} = chat_server:start_link(),
    
    %% Create client names
    ClientNames = [list_to_atom("client_" ++ integer_to_list(N)) 
                   || N <- lists:seq(1, NumClients)],
    
    %% Start and register clients
    lists:foreach(fun(Name) ->
        {ok, _} = client:start_link(Name),
        gen_server:call(Name, register)
    end, ClientNames),
    
    StartTime = erlang:system_time(microsecond),
    
    %% All clients join the room
    lists:foreach(fun(Name) ->
        gen_server:call(Name, {join_room, RoomName})
    end, ClientNames),
    
    %% All clients send messages concurrently
    Parent = self(),
    lists:foreach(fun(Name) ->
        spawn(fun() -> 
            send_messages(Name, RoomName, MessagesPerClient, Parent) 
        end)
    end, ClientNames),
    
    %% Wait for all messages to complete
    wait_for_messages(NumClients * MessagesPerClient),
    
    %% All clients leave the room
    lists:foreach(fun(Name) ->
        gen_server:call(Name, {leave_room, RoomName})
    end, ClientNames),
    
    EndTime = erlang:system_time(microsecond),
    TimeMs = (EndTime - StartTime) / 1000,
    
    TotalMessages = NumClients * MessagesPerClient,
    io:format("  Total messages: ~p~n", [TotalMessages]),
    io:format("  Completed in: ~.2f ms~n", [TimeMs]).

%% Send messages from one client
send_messages(ClientName, RoomName, NumMessages, Parent) ->
    lists:foreach(fun(N) ->
        Message = io_lib:format("Message ~p from ~p", [N, ClientName]),
        gen_server:call(ClientName, {send_message, RoomName, Message}, 10000),
        Parent ! message_sent
    end, lists:seq(1, NumMessages)).

%% Wait for all messages to complete
wait_for_messages(0) -> ok;
wait_for_messages(N) ->
    receive
        message_sent -> wait_for_messages(N - 1)
    after 10000 -> error(timeout)
    end.

%% Cleanup all processes
cleanup() ->
    %% Stop all test clients
    lists:foreach(fun(Name) ->
        case whereis(Name) of
            undefined -> ok;
            Pid -> 
                try gen_server:stop(Name) 
                catch _:_ -> exit(Pid, kill) 
                end
        end
    end, get_test_clients()),
    
    %% Stop server components
    Servers = [chat_server, client_registry],
    lists:foreach(fun(Name) ->
        case whereis(Name) of
            undefined -> ok;
            Pid -> 
                try gen_server:stop(Name) 
                catch _:_ -> exit(Pid, kill) 
                end
        end
    end, Servers),
    
    timer:sleep(100).

%% Get all test client processes
get_test_clients() ->
    [Name || Name <- registered(),
             lists:prefix("client_", atom_to_list(Name))].