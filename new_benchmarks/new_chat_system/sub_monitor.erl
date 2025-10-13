-module(sub_monitor).
-export([start_inner_omega/3]).

%% Log message to file with timestamp (duplicate for sub_monitor)
log_to_file(Message) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    Timestamp = io_lib:format("[~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w] ", 
                             [Year, Month, Day, Hour, Min, Sec]),
    LogEntry = [Timestamp, Message],
    file:write_file("atl_monitor_log.txt", LogEntry, [append]).

%% Helper function to both print and log (duplicate for sub_monitor)
print_and_log(Format, Args) ->
    Message = io_lib:format(Format, Args),
    io:format(Message),
    log_to_file(Message).

start_inner_omega(Environment, Context, ParentPID) ->
    ClientName = maps:get(clientname, Environment),
    RoomName1 = maps:get(roomname1, Environment),
    %print_and_log("SUB-MONITOR STARTED: Monitoring client ~p bound exclusively to room ~p, context ~p~n", 
                 %[ClientName, RoomName1, Context]),
    %print_and_log("Sub-monitor enforcing EXCLUSIVITY: client ~p can ONLY send messages to room ~p~n", 
                 %[ClientName, RoomName1]),
    inner_omega_loop(Environment, Context, ParentPID).

inner_omega_loop(Environment, Context, ParentPID) ->
    receive
        {client, chat_server, Msg, Context} ->
            ClientName = maps:get(clientname, Environment),
            RoomName1 = maps:get(roomname1, Environment),
            %print_and_log("SUB-MONITOR received: client -> chat_server, ~p, context ~p~n", [Msg, Context]),
            case Msg of
                {send_message, ClientName, RoomName2, Message} ->
                    %print_and_log("MESSAGE ATTEMPT: Client ~p trying to send \"~s\" to room ~p (bound to ~p), context ~p~n", 
                                 %[ClientName, Message, RoomName2, RoomName1, Context]),
                    Constraint = (maps:get(roomname1, Environment) == RoomName2),
                    if Constraint ->
                        %print_and_log("MESSAGE ALLOWED: Client ~p sent to CORRECT room ~p (matches binding), context ~p~n", 
                                     %[ClientName, RoomName2, Context]),
                        %print_and_log("Property satisfied: message \"~s\" sent to bound room~n", [Message]),
                        inner_omega_loop(Environment, Context, ParentPID)
                    ; true ->
                        %print_and_log("*** PROPERTY VIOLATION DETECTED ***~n", []),
                        %print_and_log("VIOLATION: Client ~p tried to send to room ~p but is bound to room ~p~n", 
                                     %[ClientName, RoomName2, RoomName1]),
                        %print_and_log("VIOLATION DETAILS: Message \"~s\" attempted to wrong room, context ~p~n", [Message, Context]),
                        %print_and_log("Expected room: ~p, Actual room: ~p~n", [RoomName1, RoomName2]),
                        %print_and_log("Notifying parent monitor of violation for context ~p~n", [Context]),
                        ParentPID ! {context_completed, Context, violated}
                    end;
                _ ->
                    %print_and_log("SUB-MONITOR: Ignoring non-send_message event ~p, context ~p~n", [Msg, Context]),
                    inner_omega_loop(Environment, Context, ParentPID)
            end;
        stop ->
            %print_and_log("SUB-MONITOR stopping for context ~p~n", [Context]),
            ok
    end.