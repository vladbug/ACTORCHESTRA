-module(test_context_preservation).
-export([test/0]).

test() ->
    io:format("=== Context Preservation Test ===~n"),
    io:format("This test verifies that a client maintains the same context across all operations~n"),
    
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
    
    %% Start client Alice
    io:format("4. Starting client Alice...~n"),
    {ok, _AlicePid} = client:start_link("alice"),
    
    io:format("~n=== Alice's Session - Should Use Same Context ===~n"),
    
    %% Alice performs multiple operations - all should use the same context
    io:format("5a. Alice joins general room...~n"),
    ok = client:join_room("alice", "general"),
    timer:sleep(200), % Allow context to propagate
    
    io:format("5b. Alice sends first message...~n"),
    ok = client:send_message_to_room("alice", "general", "Hello everyone!"),
    timer:sleep(200),
    
    io:format("5c. Alice joins dev room...~n"),
    ok = client:join_room("alice", "dev"),
    timer:sleep(200),
    
    io:format("5d. Alice sends message to dev room...~n"),
    ok = client:send_message_to_room("alice", "dev", "Need help with bug"),
    timer:sleep(200),
    
    io:format("5e. Alice checks her messages...~n"),
    AliceMessages1 = client:get_messages("alice"),
    io:format("   Alice received: ~p~n", [AliceMessages1]),
    timer:sleep(200),
    
    io:format("5f. Alice sends another message to general...~n"),
    ok = client:send_message_to_room("alice", "general", "Anyone there?"),
    timer:sleep(200),
    
    io:format("5g. Alice leaves general room...~n"),
    ok = client:leave_room("alice", "general"),
    timer:sleep(200),
    
    io:format("5h. Alice checks messages again...~n"),
    AliceMessages2 = client:get_messages("alice"),
    io:format("   Alice received: ~p~n", [AliceMessages2]),
    timer:sleep(200),
    
    %% Start client Bob - should get different context
    io:format("~n=== Bob's Session - Should Use Different Context ===~n"),
    io:format("6. Starting client Bob...~n"),
    {ok, _BobPid} = client:start_link("bob"),
    
    io:format("6a. Bob joins general room...~n"),
    ok = client:join_room("bob", "general"),
    timer:sleep(200),
    
    io:format("6b. Bob sends message...~n"),
    ok = client:send_message_to_room("bob", "general", "Hi from Bob!"),
    timer:sleep(200),
    
    io:format("6c. Bob checks messages...~n"),
    BobMessages = client:get_messages("bob"),
    io:format("   Bob received: ~p~n", [BobMessages]),
    timer:sleep(200),
    
    %% Alice continues with more operations - should maintain her original context
    io:format("~n=== Alice Continues - Should Maintain Original Context ===~n"),
    io:format("7a. Alice sends message to dev room again...~n"),
    ok = client:send_message_to_room("alice", "dev", "Still need help!"),
    timer:sleep(200),
    
    io:format("7b. Alice checks final messages...~n"),
    AliceMessages3 = client:get_messages("alice"),
    io:format("   Alice final messages: ~p~n", [AliceMessages3]),
    timer:sleep(200),
    
    %% Check final system state
    io:format("~n=== Final System State ===~n"),
    io:format("8. Checking final system state...~n"),
    AllRooms = server:get_rooms(),
    AllClients = server:get_all_clients(),
    GeneralClients = server:get_clients_in_room("general"),
    DevClients = server:get_clients_in_room("dev"),
    
    io:format("   All rooms: ~p~n", [AllRooms]),
    io:format("   All registered clients: ~p~n", [AllClients]),
    io:format("   General room clients: ~p~n", [GeneralClients]),
    io:format("   Dev room clients: ~p~n", [DevClients]),
    
    io:format("~n=== Context Preservation Test Complete ===~n"),
    io:format("~n=== VERIFICATION INSTRUCTIONS ===~n"),
    io:format("Look at the wrapper logs above and verify:~n"),
    io:format("1. All Alice operations (5a through 7b) use the SAME context reference~n"),
    io:format("2. All Bob operations (6a through 6c) use a DIFFERENT context reference~n"),
    io:format("3. Context is being sent back to calling processes~n"),
    io:format("4. Same client operations maintain context across different rooms~n"),
    io:format("5. Context is preserved even after client receives messages~n"),
    io:format("~nIf you see the same context reference for all Alice operations,~n"),
    io:format("then context preservation is working correctly!~n"),
    
    ok.
