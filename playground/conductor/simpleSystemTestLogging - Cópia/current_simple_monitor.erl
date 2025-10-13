-module(simple_monitor).
-export([start/0, verify_loop/0, stop/0]).

-define(LOG_FILE, "simple_monitor_log.txt").

start() ->
    case whereis(monitor) of
        undefined ->
            %% Initialize log file
            init_log_file(),
            Pid = spawn(?MODULE, verify_loop, []),
            register(monitor, Pid),
            Message = io_lib:format("Simple Monitor started with PID ~p (tracking simple add->mul chain)~n", [Pid]),
            io:format(Message),
            log_to_file(Message),
            {ok, Pid};
        Pid ->
            Message = io_lib:format("Simple Monitor already running with PID ~p~n", [Pid]),
            io:format(Message),
            log_to_file(Message),
            {ok, Pid}
    end.

stop() ->
    case whereis(monitor) of
        undefined ->
            Message = "Simple Monitor not running~n",
            io:format(Message),
            log_to_file(Message),
            ok;
        Pid ->
            Pid ! stop,
            unregister(monitor),
            Message = "Simple Monitor stopped~n",
            io:format(Message),
            log_to_file(Message),
            ok
    end.

%% Initialize log file with timestamp
init_log_file() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    Header = io_lib:format("=== Simple Monitor Log Started at ~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w ===~n~n", 
                          [Year, Month, Day, Hour, Min, Sec]),
    file:write_file(?LOG_FILE, Header, [write]).

%% Log message to file with timestamp
log_to_file(Message) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    Timestamp = io_lib:format("[~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w] ", 
                             [Year, Month, Day, Hour, Min, Sec]),
    LogEntry = [Timestamp, Message],
    file:write_file(?LOG_FILE, LogEntry, [append]).

%% Helper function to both print and log
print_and_log(Format, Args) ->
    Message = io_lib:format(Format, Args),
    io:format(Message),
    log_to_file(Message).

verify_loop() ->
    receive
        stop ->
            print_and_log("Simple Monitor stopping...~n", []),
            ok;
        
        %% Step 1: Client -> Add Server with {add_ten, Number}
        {simple_client, add_server, {add_ten, Number}, Context} ->
            print_and_log("Monitor received [1/4]: simple_client -> add_server, add_ten(~p), Context ~p~n", 
                         [Number, Context]),
            receive
                stop ->
                    print_and_log("Simple Monitor stopping...~n", []),
                    ok;
                
                %% Step 2: Add Server -> Mul Server with {multiply_two, Number+10}
                {add_server, mul_server, {multiply_two, AddedNumber}, Context} ->
                    print_and_log("Monitor received [2/4]: add_server -> mul_server, multiply_two(~p), Context ~p~n", 
                                 [AddedNumber, Context]),
                    receive
                        stop ->
                            print_and_log("Simple Monitor stopping...~n", []),
                            ok;
                        
                        %% Step 3: Mul Server -> Add Server with result
                        {mul_server, add_server, {ok, MulResult}, Context} ->
                            print_and_log("Monitor received [3/4]: mul_server -> add_server, result ~p, Context ~p~n", 
                                         [MulResult, Context]),
                            receive
                                stop ->
                                    print_and_log("Simple Monitor stopping...~n", []),
                                    ok;
                                
                                %% Step 4: Add Server -> Client with final result
                                {add_server, simple_client, {ok, FinalResult}, Context} ->
                                    print_and_log("Monitor received [4/4]: add_server -> simple_client, result ~p, Context ~p~n", 
                                                 [FinalResult, Context]),
                                    
                                    %% Verify context consistency and show complete processing chain
                                    print_and_log("✓ CONTEXT PRESERVED: ~p throughout entire simple chain~n", [Context]),
                                    
                                    %% Show the complete processing flow
                                    print_and_log("=== COMPLETE SIMPLE PROCESSING FLOW ===~n", []),
                                    print_and_log("1. simple_client -> add_server: add_ten(~p) (Context: ~p)~n", [Number, Context]),
                                    print_and_log("2. add_server -> mul_server: multiply_two(~p) (Context: ~p)~n", [AddedNumber, Context]),
                                    print_and_log("3. mul_server -> add_server: result ~p (Context: ~p)~n", [MulResult, Context]),
                                    print_and_log("4. add_server -> simple_client: result ~p (Context: ~p)~n", [FinalResult, Context]),
                                    print_and_log("   Simple chain: ~p -> +10 -> ~p -> *2 -> ~p~n", [Number, AddedNumber, FinalResult]),
                                    print_and_log("=== END SIMPLE PROCESSING FLOW ===~n~n", []),
                                    
                                    %% Continue monitoring (recursive call)
                                    verify_loop()
                            after 5000 ->
                                print_and_log("✗ TIMEOUT: Expected final result (add_server->client) but got none after 5 seconds~n", []),
                                verify_loop()
                            end
                    after 5000 ->
                        print_and_log("✗ TIMEOUT: Expected result (mul_server->add_server) but got none after 5 seconds~n", []),
                        verify_loop()
                    end
            after 5000 ->
                print_and_log("✗ TIMEOUT: Expected 2nd message (add_server->mul_server) but got none after 5 seconds~n", []),
                verify_loop()
            end;
        
        %% Alternative: If we get different flow patterns, log them for debugging
        {FromModule, ToModule, Message, Context} ->
            print_and_log("Monitor received unexpected flow: ~p -> ~p, message ~p, Context ~p~n", 
                         [FromModule, ToModule, Message, Context]),
            verify_loop();
            
        %% Catch-all for debugging
        UnknownMessage ->
            print_and_log("Monitor received unknown message: ~p~n", [UnknownMessage]),
            verify_loop()
    after 30000 ->
        print_and_log("Simple Monitor waiting for processing messages... (will timeout in 30s if no activity)~n", []),
        verify_loop()
    end.