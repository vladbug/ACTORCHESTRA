-module(test_communication).
-export([test/0]).

test() ->
    io:format("=== Testing Server <-> Register Communication ===~n"),
    
    %% Start the wrapper first (required by context_injector)
    io:format("1. Starting wrapper...~n"),
    {ok, _WrapperPid} = wrapper:start_link(),
    
    %% Start the server (which automatically starts the register module)
    io:format("2. Starting server...~n"),
    {ok, _ServerPid} = server:start_link(),
    
    %% Test room registration
    io:format("3. Registering rooms...~n"),
    ok = server:register_room("general"),
    ok = server:register_room("random"),
    ok = server:register_room("dev"),
    
    %% Test getting rooms
    io:format("4. Getting registered rooms...~n"),
    Rooms = server:get_rooms(),
    io:format("   Registered rooms: ~p~n", [Rooms]),
    
    %% Test client registration
    io:format("5. Registering clients...~n"),
    server:register_client("alice", "general"),
    server:register_client("bob", "general"),
    server:register_client("charlie", "dev"),
    server:register_client("dave", "random"),
    
    %% Wait a bit for cast messages to be processed
    timer:sleep(100),
    
    %% Test getting clients in specific room
    io:format("6. Getting clients in 'general' room...~n"),
    GeneralClients = server:get_clients_in_room("general"),
    io:format("   Clients in 'general': ~p~n", [GeneralClients]),
    
    %% Test getting all clients
    io:format("7. Getting all registered clients...~n"),
    AllClients = server:get_all_clients(),
    io:format("   All clients: ~p~n", [AllClients]),
    
    %% Test registering client in non-existent room
    io:format("8. Testing registration in non-existent room...~n"),
    server:register_client("eve", "non_existent"),
    
    timer:sleep(100),
    
    %% Check if eve was registered
    AllClientsAfter = server:get_all_clients(),
    io:format("   All clients after failed registration: ~p~n", [AllClientsAfter]),
    
    io:format("=== Communication Test Complete ===~n"),
    ok.
