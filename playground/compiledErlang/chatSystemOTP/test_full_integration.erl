-module(test_full_integration).
-export([test/0]).

test() ->
    io:format("=== Full System Integration Test ===~n"),
    
    %% Start the wrapper first (required by context_injector)
    io:format("1. Starting wrapper...~n"),
    {ok, _WrapperPid} = wrapper:start_link(),
    
    %% Start the server (which automatically starts the register module)
    io:format("2. Starting server...~n"),
    {ok, _ServerPid} = server:start_link(),
    
    %% Create some rooms
    io:format("3. Creating chat rooms...~n"),
    ok = server:register_room("general"),
    ok = server:register_room("dev"),
    ok = server:register_room("random"),
    
    %% Start some clients
    io:format("4. Starting clients...~n"),
    {ok, _AlicePid} = client:start_link("alice"),
    {ok, _BobPid} = client:start_link("bob"),
    {ok, _CharliePid} = client:start_link("charlie"),
    
    %% Have clients join rooms
    io:format("5. Clients joining rooms...~n"),
    ok = client:join_room("alice", "general"),
    ok = client:join_room("bob", "general"),
    ok = client:join_room("charlie", "dev"),
    ok = client:join_room("alice", "dev"),  % Alice joins multiple rooms
    
    timer:sleep(100), % Let registration settle
    
    %% Check room memberships
    io:format("6. Checking room memberships...~n"),
    GeneralClients = server:get_clients_in_room("general"),
    DevClients = server:get_clients_in_room("dev"),
    io:format("   General room clients: ~p~n", [GeneralClients]),
    io:format("   Dev room clients: ~p~n", [DevClients]),
    
    %% Send messages to rooms
    io:format("7. Sending messages to rooms...~n"),
    ok = client:send_message_to_room("alice", "general", "Hello everyone!"),
    ok = client:send_message_to_room("bob", "general", "Hi Alice!"),
    ok = client:send_message_to_room("charlie", "dev", "Working on some code..."),
    ok = client:send_message_to_room("alice", "dev", "Need help with debugging"),
    
    timer:sleep(200), % Let messages propagate
    
    %% Check client messages
    io:format("8. Checking client messages...~n"),
    AliceMessages = client:get_messages("alice"),
    BobMessages = client:get_messages("bob"),
    CharlieMessages = client:get_messages("charlie"),
    
    io:format("   Alice received: ~p~n", [AliceMessages]),
    io:format("   Bob received: ~p~n", [BobMessages]),
    io:format("   Charlie received: ~p~n", [CharlieMessages]),
    
    %% Test leaving rooms
    io:format("9. Testing room leave functionality...~n"),
    ok = client:leave_room("alice", "general"),
    
    timer:sleep(100),
    
    %% Send another message to general room
    io:format("10. Sending message after Alice left general...~n"),
    ok = client:send_message_to_room("bob", "general", "Alice left the room"),
    
    timer:sleep(200),
    
    %% Check messages again
    io:format("11. Final message check...~n"),
    AliceMessages2 = client:get_messages("alice"),
    BobMessages2 = client:get_messages("bob"),
    
    io:format("   Alice received after leaving general: ~p~n", [AliceMessages2]),
    io:format("   Bob received: ~p~n", [BobMessages2]),
    
    %% Show final system state
    io:format("12. Final system state...~n"),
    AllRooms = server:get_rooms(),
    AllClients = server:get_all_clients(),
    io:format("   All rooms: ~p~n", [AllRooms]),
    io:format("   All registered clients: ~p~n", [AllClients]),
    
    io:format("=== Integration Test Complete ===~n"),
    ok.
