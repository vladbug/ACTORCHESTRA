-module(baseline_chat_room).
-behaviour(gen_server).

%% Only startup function exported, no API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    room_name,
    members = [] %% List of {ClientName, ClientPid} tuples
}).

%%% Startup only

start_link(RoomName) ->
    gen_server:start_link(?MODULE, [RoomName], []).

%%% gen_server callbacks

init([RoomName]) ->
    {ok, #state{room_name = RoomName}}.

%% Raw handle_calls only - no API functions

handle_call({add_member, ClientName, ClientPid}, _From, State) ->
    Members = State#state.members,
    RoomName = State#state.room_name,
    
    %% No verification - just add member directly
    %% Remove if already exists to avoid duplicates
    NewMembers = [{ClientName, ClientPid} | 
                 lists:keydelete(ClientName, 1, Members)],
    
    io:format("[ROOM ~p] Client ~p joined~n", [RoomName, ClientName]),
    {reply, ok, State#state{members = NewMembers}};

handle_call({remove_member, ClientName}, _From, State) ->
    Members = State#state.members,
    RoomName = State#state.room_name,
    
    %% Remove member
    NewMembers = lists:keydelete(ClientName, 1, Members),
    
    io:format("[ROOM ~p] Client ~p left~n", [RoomName, ClientName]),
    {reply, ok, State#state{members = NewMembers}};

% handle_call({broadcast_message, _FromClient, _Message}, From, State) ->
%     %RoomName = State#state.room_name,
%     %io:format("[ROOM ~p] ~p: ~s~n", [RoomName, FromClient, Message]),
%     spawn(fun() ->
%         %% simulate "work": sum numbers 1..1_000_000
%         Sum = lists:sum(lists:seq(1, 100000)),
%         io:format("Async sum result: ~p~n", [Sum]),
%         gen_server:reply(From, ok)
%     end),
%     {noreply, State};
%     %{reply, ok, State};

handle_call({broadcast_message, FromClient, Message}, From, State) ->
    Members = State#state.members,
    RoomName = State#state.room_name,
    
    %% ASYNC: Spawn worker to handle message broadcasting
    spawn(fun() ->
        %% IMPORTANT: No verification if FromClient is actually a member
        %% This intentionally allows violations
        
        %% Broadcast message to ALL members EXCEPT the sender using gen_server:call
        lists:foreach(fun({MemberName, MemberPid}) ->
            case MemberName of
                FromClient ->
                    %% Don't call back to sender (would cause deadlock)
                    ok;
                _ ->
                    try
                        gen_server:cast(MemberPid, {receive_message, RoomName, FromClient, Message})
                    catch
                        _:_ ->
                            %% Member process might be down, ignore
                            ok
                    end
            end
        end, Members),
        
        %% Log the message for debugging
        io:format("[ROOM ~p] ~p: ~s~n", [RoomName, FromClient, Message]),
        
        gen_server:reply(From, ok)
    end),
    
    {noreply, State};

handle_call(get_members, _From, State) ->
    %% Return list of member names for debugging
    MemberNames = [Name || {Name, _Pid} <- State#state.members],
    {reply, MemberNames, State};

handle_call(get_room_info, _From, State) ->
    %% Return room info for debugging
    Info = #{
        room_name => State#state.room_name,
        member_count => length(State#state.members),
        members => [Name || {Name, _Pid} <- State#state.members]
    },
    {reply, Info, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    RoomName = State#state.room_name,
    io:format("[ROOM ~p] Room shutting down~n", [RoomName]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.