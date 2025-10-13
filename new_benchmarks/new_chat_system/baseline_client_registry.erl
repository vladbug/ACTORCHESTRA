-module(baseline_client_registry).
-behaviour(gen_server).

%% Only startup function exported, no API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    clients = #{},      %% #{ClientName => ClientPid}
    room_memberships = #{} %% #{ClientName => [RoomName1, RoomName2, ...]}
}).

%%% Startup only

start_link() ->
    gen_server:start_link({local, baseline_client_registry}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

%% Raw handle_calls only - no API functions

handle_call({add_client, ClientName, ClientPid}, _From, State) ->
    Clients = State#state.clients,
    RoomMemberships = State#state.room_memberships,
    
    %% Add client to registry
    NewClients = Clients#{ClientName => ClientPid},
    NewRoomMemberships = RoomMemberships#{ClientName => []},
    
    io:format("[REGISTRY] Client ~p registered~n", [ClientName]),
    {reply, ok, State#state{clients = NewClients, room_memberships = NewRoomMemberships}};

handle_call({remove_client, ClientName}, _From, State) ->
    Clients = State#state.clients,
    RoomMemberships = State#state.room_memberships,
    
    %% Remove client from registry
    NewClients = maps:remove(ClientName, Clients),
    NewRoomMemberships = maps:remove(ClientName, RoomMemberships),
    
    io:format("[REGISTRY] Client ~p unregistered~n", [ClientName]),
    {reply, ok, State#state{clients = NewClients, room_memberships = NewRoomMemberships}};

handle_call({join_room, ClientName, RoomName}, _From, State) ->
    RoomMemberships = State#state.room_memberships,
    
    %% Add room to client's membership list
    CurrentRooms = maps:get(ClientName, RoomMemberships, []),
    NewRooms = case lists:member(RoomName, CurrentRooms) of
        true -> CurrentRooms; % Already in room
        false -> [RoomName | CurrentRooms]
    end,
    NewRoomMemberships = RoomMemberships#{ClientName => NewRooms},
    
    io:format("[REGISTRY] Client ~p joined room ~p~n", [ClientName, RoomName]),
    {reply, ok, State#state{room_memberships = NewRoomMemberships}};

handle_call({leave_room, ClientName, RoomName}, _From, State) ->
    RoomMemberships = State#state.room_memberships,
    
    %% Remove room from client's membership list
    CurrentRooms = maps:get(ClientName, RoomMemberships, []),
    NewRooms = lists:delete(RoomName, CurrentRooms),
    NewRoomMemberships = RoomMemberships#{ClientName => NewRooms},
    
    io:format("[REGISTRY] Client ~p left room ~p~n", [ClientName, RoomName]),
    {reply, ok, State#state{room_memberships = NewRoomMemberships}};

handle_call({get_client_rooms, ClientName}, _From, State) ->
    RoomMemberships = State#state.room_memberships,
    Rooms = maps:get(ClientName, RoomMemberships, []),
    {reply, Rooms, State};

handle_call({is_client_in_room, ClientName, RoomName}, _From, State) ->
    RoomMemberships = State#state.room_memberships,
    ClientRooms = maps:get(ClientName, RoomMemberships, []),
    IsInRoom = lists:member(RoomName, ClientRooms),
    {reply, IsInRoom, State};

handle_call({get_all_clients}, _From, State) ->
    Clients = State#state.clients,
    {reply, maps:keys(Clients), State};

handle_call({get_room_members, RoomName}, _From, State) ->
    RoomMemberships = State#state.room_memberships,
    Members = maps:fold(fun(ClientName, Rooms, Acc) ->
        case lists:member(RoomName, Rooms) of
            true -> [ClientName | Acc];
            false -> Acc
        end
    end, [], RoomMemberships),
    {reply, Members, State};

handle_call(get_registry_state, _From, State) ->
    %% Debug function to see full state
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.