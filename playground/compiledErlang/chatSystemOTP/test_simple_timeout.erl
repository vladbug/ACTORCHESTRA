-module(test_simple_timeout).
-export([test/0]).

test() ->
    io:format("=== Simple Timeout Debug Test ===~n"),
    
    %% Start the wrapper first
    io:format("1. Starting wrapper...~n"),
    {ok, _WrapperPid} = wrapper:start_link(),
    
    %% Start the server
    io:format("2. Starting server...~n"),
    {ok, _ServerPid} = server:start_link(),
    
    %% Just try ONE simple operation
    io:format("3. Attempting single room registration...~n"),
    
    %% Set a custom timeout for debugging
    try
        Result = gen_server:call(server, {register_room, "debug_room"}, 10000), % 10 second timeout
        io:format("4. Room registration result: ~p~n", [Result])
    catch
        exit:{timeout, _} ->
            io:format("4. TIMEOUT occurred during room registration~n");
        Error:Reason ->
            io:format("4. ERROR occurred: ~p:~p~n", [Error, Reason])
    end,
    
    io:format("=== Simple Test Complete ===~n"),
    ok.
