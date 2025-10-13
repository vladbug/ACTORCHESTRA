-module(chat_server).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    registry_pid,  %% pid of client_registry
    rooms = #{}    %% #{RoomName => RoomPid}
}).

start_link() ->
    gen_server:start_link({local, chat_server}, ?MODULE, [], []).

init([]) ->
    {ok, RegistryPid} = client_registry:start_link(),
    {ok, #state{registry_pid = RegistryPid}}.

handle_call({register_client, ClientName, ClientPid}, From, State) ->
    RegistryPid = State#state.registry_pid,
    spawn(fun() ->
        _Result = gen_server:call(RegistryPid, {add_client, ClientName, ClientPid}),
        gen_server:reply(From, {ok, registered})
    end),
    {noreply, State};

handle_call({join_room, ClientName, RoomName}, From, State) ->
    Rooms = State#state.rooms,
    RegistryPid = State#state.registry_pid,

    spawn(fun() ->
        %% Get or create room
        RoomPid = case maps:get(RoomName, Rooms, undefined) of
            undefined ->
                {ok, NewRoomPid} = chat_room:start_link(RoomName),
                gen_server:call(chat_server, {register_room, RoomName, NewRoomPid}),
                NewRoomPid;
            ExistingRoomPid ->
                ExistingRoomPid
        end,
        
        gen_server:call(RoomPid, {add_member, ClientName, whereis(ClientName)}),
        gen_server:call(RegistryPid, {join_room, ClientName, RoomName}),
        
        gen_server:reply(From, {ok, joined})
    end),
    
    {noreply, State};

handle_call({send_message, ClientName, RoomName, Message}, From, State) ->
    Rooms = State#state.rooms,
    
    spawn(fun() ->
        %% NO VERIFICATION - intentionally allow messages to any room
        case maps:get(RoomName, Rooms, undefined) of
            undefined ->
                %% Room doesn't exist, create it and send message anyway
                {ok, RoomPid} = chat_room:start_link(RoomName),
                gen_server:call(chat_server, {register_room, RoomName, RoomPid}),
                gen_server:call(RoomPid, {broadcast_message, ClientName, Message}),
                gen_server:reply(From, {ok, message_sent});
            RoomPid ->
                %% Room exists, send message
                gen_server:call(RoomPid, {broadcast_message, ClientName, Message}),
                gen_server:reply(From, {ok, message_sent})
        end
    end),
    {noreply, State};

handle_call({leave_room, ClientName, RoomName}, From, State) ->
    Rooms = State#state.rooms,
    RegistryPid = State#state.registry_pid,
    
    spawn(fun() ->
        case maps:get(RoomName, Rooms, undefined) of
            undefined ->
                gen_server:reply(From, {error, room_not_found});
            RoomPid ->
                gen_server:call(RoomPid, {remove_member, ClientName}),
                gen_server:call(RegistryPid, {leave_room, ClientName, RoomName}),
                gen_server:reply(From, {ok, left})
        end
    end),
    {noreply, State};

handle_call({register_room, RoomName, RoomPid}, _From, State) ->
    Rooms = State#state.rooms,
    NewRooms = Rooms#{RoomName => RoomPid},
    {reply, ok, State#state{rooms = NewRooms}};

handle_call(get_rooms, _From, State) ->
    {reply, maps:keys(State#state.rooms), State};

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