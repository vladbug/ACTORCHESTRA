-module(monitor).
-export([start/0, stop/0, monitor_loop/0]).

-define(LOG_FILE, "monitor_log.txt").

start() ->
    case whereis(monitor) of
        undefined ->
            %% Initialize log file
            init_log_file(),
            Pid = spawn(?MODULE, monitor_loop, []),
            register(monitor, Pid),
            Message = io_lib:format("ATL Monitor started with PID ~p (tracking room exclusivity property)~n", [Pid]),
            io:format(Message),
            log_to_file(Message),
            {ok, Pid};
        Pid ->
            Message = io_lib:format("ATL Monitor already running with PID ~p~n", [Pid]),
            io:format(Message),
            log_to_file(Message),
            {ok, Pid}
    end.

stop() ->
    case whereis(monitor) of
        undefined ->
            Message = "ATL Monitor not running~n",
            io:format(Message),
            log_to_file(Message),
            ok;
        Pid ->
            Pid ! stop,
            unregister(monitor),
            Message = "ATL Monitor stopped~n",
            io:format(Message),
            log_to_file(Message),
            ok
    end.

%% Initialize log file with timestamp
init_log_file() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    Header = io_lib:format("=== ATL Monitor Log Started at ~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w ===~n", 
                          [Year, Month, Day, Hour, Min, Sec]),
    Header2 = io_lib:format("=== PROPERTY: Once joined to a room, client can ONLY send messages to that room ===~n~n", []),
    file:write_file(?LOG_FILE, [Header, Header2], [write]).

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

monitor_loop() ->
    monitor_loop(#{}).

monitor_loop(ContextMap) ->
    receive
        stop ->
            print_and_log("ATL Monitor stopping... terminating all sub-monitors~n", []),
            maps:fold(fun(Context, PID, _) -> 
                print_and_log("Stopping sub-monitor for context ~p~n", [Context]),
                PID ! stop 
            end, ok, ContextMap),
            ok;
        {Src, Dst, Msg, Context} ->
            print_and_log("ATL Monitor received: ~p -> ~p, message ~p, context ~p~n", [Src, Dst, Msg, Context]),
            case maps:get(Context, ContextMap, undefined) of
                undefined ->
                    print_and_log("New context ~p detected, starting main loop~n", [Context]),
                    mainLoop1(Src, Dst, Msg, Context, ContextMap);
                PID ->
                    print_and_log("Forwarding to existing sub-monitor for context ~p~n", [Context]),
                    PID ! {Src, Dst, Msg, Context},
                    monitor_loop(ContextMap)
            end;
        {context_completed, Context, Result} ->
            print_and_log("Context ~p completed with result: ~p~n", [Context, Result]),
            NewContextMap = maps:remove(Context, ContextMap),
            case Result of
                violated -> 
                    print_and_log("*** PROPERTY VIOLATION DETECTED *** in context ~p~n", [Context]),
                    violated;
                _ -> 
                    print_and_log("Context ~p completed successfully~n", [Context]),
                    monitor_loop(NewContextMap)
            end
    end.

mainLoop1(Src, Dst, Msg, Context, ContextMap) ->
    print_and_log("Main loop processing: ~p -> ~p, ~p, context ~p~n", [Src, Dst, Msg, Context]),
    case {Src, Dst, Msg} of
        {client, chat_server, {join_room, ClientName, RoomName1}} ->
            print_and_log("Step 1 MATCHED: Client ~p requesting to join room ~p, context ~p~n", [ClientName, RoomName1, Context]),
            Constraint = true,
            if Constraint ->
                print_and_log("Step 1 SATISFIED: Constraint passed for client ~p, room ~p, context ~p~n", [ClientName, RoomName1, Context]),
                % First step satisfied, continue with remaining chain
                print_and_log("Waiting for join confirmation for client ~p, room ~p, context ~p~n", [ClientName, RoomName1, Context]),
                receive
                    {chat_server, client, Msg2, Context} ->
                        print_and_log("Step 2 received: chat_server -> client, ~p, context ~p~n", [Msg2, Context]),
                        case Msg2 of
                            {ok, joined} ->
                                print_and_log("Step 2 SATISFIED: Join confirmed for client ~p, room ~p, context ~p~n", [ClientName, RoomName1, Context]),
                                Constraint = true,
                                if Constraint ->
                                    print_and_log("Both steps satisfied - SPAWNING SUB-MONITOR for context ~p~n", [Context]),
                                    % Final step satisfied - spawn sub-monitor
                                    Environment = #{clientname => ClientName, roomname1 => RoomName1},
                                    print_and_log("Sub-monitor environment: ~p~n", [Environment]),
                                    print_and_log("Client ~p is now bound exclusively to room ~p~n", [ClientName, RoomName1]),
                                    SubPID = spawn(sub_monitor, start_inner_omega, [Environment, Context, self()]),
                                    NewContextMap = ContextMap#{Context => SubPID},
                                    print_and_log("Sub-monitor spawned with PID ~p for context ~p~n", [SubPID, Context]),
                                    monitor_loop(NewContextMap)
                                ; true ->
                                    print_and_log("Step 2 VIOLATED: Final constraint failed, context ~p~n", [Context]),
                                    violated % Final step violated
                                end;
                            _ ->
                                print_and_log("Step 2 MISMATCH: Expected {ok, joined}, got ~p, restarting context ~p~n", [Msg, Context]),
                                mainLoop1(Src, Dst, Msg2, Context, ContextMap) % Pattern mismatch, restart
                        end
                end
            ; true ->
                print_and_log("Step 1 VIOLATED: First constraint failed, context ~p~n", [Context]),
                violated % First step violated
            end;
        _ ->
            print_and_log("PATTERN MISMATCH: Expected {client, chat_server, {join_room, _, _}}, got {~p, ~p, ~p}, context ~p~n", 
                         [Src, Dst, Msg, Context]),
            monitor_loop(ContextMap) % Pattern mismatch
    end.