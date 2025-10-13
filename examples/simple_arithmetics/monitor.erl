-module(monitor).
-export([start/0, verify_loop/0, stop/0]).

-define(LOG_FILE, "monitor_log.txt").

start() ->
    case whereis(monitor) of
        undefined ->
            %% Initialize log file
            init_log_file(),
            Pid = spawn(?MODULE, verify_loop, []),
            register(monitor, Pid),
            Message = io_lib:format("Monitor started with PID ~p (tracking module-to-module flow)~n", [Pid]),
            %io:format(Message),
            log_to_file(Message),
            {ok, Pid};
        Pid ->
            Message = io_lib:format("Monitor already running with PID ~p~n", [Pid]),
            io:format(Message),
            log_to_file(Message),
            {ok, Pid}
    end.

stop() ->
    case whereis(monitor) of
        undefined ->
            Message = "Monitor not running~n",
            io:format(Message),
            log_to_file(Message),
            ok;
        Pid ->
            Pid ! stop,
            unregister(monitor),
            Message = "Monitor stopped~n",
            %io:format(Message),
            log_to_file(Message),
            ok
    end.

%% Initialize log file with timestamp
init_log_file() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    Header = io_lib:format("=== Monitor Log Started at ~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w ===~n~n", 
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
    %io:format(Message),
    log_to_file(Message).

verify_loop() ->
    receive
        stop ->
            %print_and_log("Monitor stopping...~n", []),
            ok;
        %% Step 1: Client -> Central Server with {process, ClientPid, Number}
        %% Now tracking module-to-module flow instead of PID-to-PID
        {client, slow_central_server, {process, ClientPid, Number}, Context} ->
            print_and_log("Monitor received [1/4]: client -> slow_central_server, process(~p, ~p), Context ~p~n", 
                         [ClientPid, Number, Context]),
            receive
                stop ->
                    print_and_log("Monitor stopping...~n", []),
                    ok;
                %% Step 2: Central Server -> Add10 with {process, Number}
                {slow_central_server, slow_proc_add10, {process, ProcessNumber}, Context} ->
                    print_and_log("Monitor received [2/4]: slow_central_server -> slow_proc_add10, process ~p, Context ~p~n", 
                                 [ProcessNumber, Context]),
                    receive
                        stop ->
                            print_and_log("Monitor stopping...~n", []),
                            ok;
                        %% Step 3: Add10 -> Mul2 with {process, Number+10}
                        {slow_proc_add10, slow_proc_mul2, {process, AddedNumber}, Context} ->
                            print_and_log("Monitor received [3/4]: slow_proc_add10 -> slow_proc_mul2, process ~p, Context ~p~n", 
                                         [AddedNumber, Context]),
                            receive
                                stop ->
                                    print_and_log("Monitor stopping...~n", []),
                                    ok;
                                %% Step 4: Final result back through the chain
                                %% Multiple possible return paths, we'll catch them all
                                {slow_proc_mul2, slow_proc_add10, {ok, Result1}, Context} ->
                                    print_and_log("Monitor received [4a/5]: slow_proc_mul2 -> slow_proc_add10, result ~p, Context ~p~n", 
                                                 [Result1, Context]),
                                    receive
                                        stop ->
                                            print_and_log("Monitor stopping...~n", []),
                                            ok;
                                        {slow_proc_add10, slow_central_server, {ok, Result2}, Context} ->
                                            print_and_log("Monitor received [4b/5]: slow_proc_add10 -> slow_central_server, result ~p, Context ~p~n", 
                                                         [Result2, Context]),
                                            receive
                                                stop ->
                                                    print_and_log("Monitor stopping...~n", []),
                                                    ok;
                                                {slow_central_server, client, {ok, FinalResult}, Context} ->
                                                    print_and_log("Monitor received [5/5]: slow_central_server -> client, result ~p, Context ~p~n", 
                                                                 [FinalResult, Context]),
                                                    
                                                    %% Show the complete processing flow with module names
                                                    print_and_log("=== COMPLETE MODULE-TO-MODULE PROCESSING FLOW ===~n", []),
                                                    print_and_log("1. client -> slow_central_server: process(~p, ~p) (Context: ~p)~n", [ClientPid, Number, Context]),
                                                    print_and_log("2. slow_central_server -> slow_proc_add10: process ~p (Context: ~p)~n", [ProcessNumber, Context]),
                                                    print_and_log("3. slow_proc_add10 -> slow_proc_mul2: process ~p (Context: ~p)~n", [AddedNumber, Context]),
                                                    print_and_log("4. slow_proc_mul2 -> slow_proc_add10: result ~p (Context: ~p)~n", [Result1, Context]),
                                                    print_and_log("5. slow_proc_add10 -> slow_central_server: result ~p (Context: ~p)~n", [Result2, Context]),
                                                    print_and_log("6. slow_central_server -> client: result ~p (Context: ~p)~n", [FinalResult, Context]),
                                                    print_and_log("   Processing chain: ~p -> +10 -> ~p -> *2 -> ~p~n", [Number, AddedNumber, FinalResult]),
                                                    print_and_log("=== END PROCESSING FLOW ===~n~n", []),
                                                    
                                                    %% Continue monitoring
                                                    verify_loop()
                                            after 5000 ->
                                                print_and_log("✗ TIMEOUT: Expected final result (central->client) but got none after 5 seconds~n", []),
                                                verify_loop()
                                            end
                                    after 5000 ->
                                        print_and_log("✗ TIMEOUT: Expected result (add10->central) but got none after 5 seconds~n", []),
                                        verify_loop()
                                    end
                            after 5000 ->
                                print_and_log("✗ TIMEOUT: Expected result (mul2->add10) but got none after 5 seconds~n", []),
                                verify_loop()
                            end
                    after 5000 ->
                        print_and_log("✗ TIMEOUT: Expected 3rd message (add10->mul2) but got none after 5 seconds~n", []),
                        verify_loop()
                    end
            after 5000 ->
                print_and_log("✗ TIMEOUT: Expected 2nd message (central->add10) but got none after 5 seconds~n", []),
                verify_loop()
            end;
        
        %% Alternative: If we get different flow patterns, log them for debugging
        {FromModule, ToModule, Message, Context} ->
            print_and_log("Monitor received unexpected flow: ~p -> ~p, message ~p, Context ~p~n", 
                         [FromModule, ToModule, Message, Context]),
            verify_loop();
            
        {_FromPid, _ToPid, _Message, _Context} when is_pid(_FromPid) ->
            print_and_log("Monitor received legacy PID-based message (ignoring - expecting module-based flow)~n", []),
            verify_loop();
        
        %% Catch-all for debugging
        UnknownMessage ->
            print_and_log("Monitor received unknown message: ~p~n", [UnknownMessage]),
            verify_loop()
    after 30000 ->
        print_and_log("Monitor waiting for processing messages... (will timeout in 30s if no activity)~n", []),
        verify_loop()
    end.