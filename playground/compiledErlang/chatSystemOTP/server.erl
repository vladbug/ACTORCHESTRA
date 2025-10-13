% The server.erl will communicate with this register module to register clients into chat rooms

-module(server).
-behaviour(gen_server).
-export([start_link/0, register_room/1, get_rooms/0, register_client/2, get_clients_in_room/1, get_all_clients/0, join_room/2, leave_room/2, send_message_to_room/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile({parse_transform, context_injector}).

-record(state, {
    register_pid = undefined
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_room(RoomName) ->
    gen_server:call(?MODULE, {register_room, RoomName}).

get_rooms() ->
    gen_server:call(?MODULE, {get_rooms}).

register_client(ClientName, RoomName) ->
    gen_server:cast(?MODULE, {register_client, ClientName, RoomName}).

get_clients_in_room(RoomName) ->
    gen_server:call(?MODULE, {get_clients_in_room, RoomName}).

get_all_clients() ->
    gen_server:call(?MODULE, {get_all_clients}).

join_room(ClientName, RoomName) ->
    gen_server:call(?MODULE, {join_room, ClientName, RoomName}).

leave_room(ClientName, RoomName) ->
    gen_server:call(?MODULE, {leave_room, ClientName, RoomName}).

send_message_to_room(ClientName, RoomName, Message) ->
    gen_server:call(?MODULE, {send_message_to_room, ClientName, RoomName, Message}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[server] Starting~n"),
    %% Start the register module
    {ok, RegisterPid} = register:start_link(),
    io:format("[server] Started register module with PID: ~p~n", [RegisterPid]),
    {ok, #state{register_pid = RegisterPid}}.

handle_call({register_room, RoomName}, _From, State) ->
    io:format("[server] Delegating room registration to register module: ~p~n", [RoomName]),
    %% First register the room in the register module
    Result = gen_server:call(register, {register_room, RoomName}),
    case Result of
        ok ->
            %% Start the room process
            RoomAtom = list_to_atom(RoomName),
            case room:start_link(RoomAtom) of
                {ok, _RoomPid} ->
                    io:format("[server] Started room process for ~p~n", [RoomName]),
                    {reply, ok, State};
                {error, Reason} ->
                    io:format("[server] Failed to start room process for ~p: ~p~n", [RoomName, Reason]),
                    {reply, {error, Reason}, State}
            end;
        Error ->
            {reply, Error, State}
    end;

handle_call({get_rooms}, _From, State) ->
    io:format("[server] Delegating room retrieval to register module~n"),
    %% Delegate to the register module
    Rooms = gen_server:call(register, {get_rooms}),
    {reply, Rooms, State};

handle_call({get_clients_in_room, RoomName}, _From, State) ->
    io:format("[server] Delegating client retrieval for room ~p to register module~n", [RoomName]),
    %% Delegate to the register module
    Clients = gen_server:call(register, {get_clients_in_room, RoomName}),
    {reply, Clients, State};

handle_call({get_all_clients}, _From, State) ->
    io:format("[server] Delegating all clients retrieval to register module~n"),
    %% Delegate to the register module
    AllClients = gen_server:call(register, {get_all_clients}),
    {reply, AllClients, State};

handle_call({join_room, ClientName, RoomName}, _From, State) ->
    io:format("[server] Client ~p joining room ~p~n", [ClientName, RoomName]),
    %% First register the client in the register module
    gen_server:cast(register, {register_client, ClientName, RoomName}),
    %% Then add the client to the actual room process
    RoomAtom = list_to_atom(RoomName),
    case whereis(RoomAtom) of
        undefined ->
            {reply, {error, room_not_found}, State};
        _RoomPid ->
            Result = room:join_room(ClientName, RoomAtom),
            {reply, Result, State}
    end;

handle_call({leave_room, ClientName, RoomName}, _From, State) ->
    io:format("[server] Client ~p leaving room ~p~n", [ClientName, RoomName]),
    RoomAtom = list_to_atom(RoomName),
    case whereis(RoomAtom) of
        undefined ->
            {reply, {error, room_not_found}, State};
        _RoomPid ->
            Result = room:leave_room(ClientName, RoomAtom),
            {reply, Result, State}
    end;

handle_call({send_message_to_room, ClientName, RoomName, Message}, _From, State) ->
    io:format("[server] Client ~p sending message to room ~p: ~p~n", [ClientName, RoomName, Message]),
    RoomAtom = list_to_atom(RoomName),
    case whereis(RoomAtom) of
        undefined ->
            {reply, {error, room_not_found}, State};
        _RoomPid ->
            Result = room:send_message(ClientName, RoomAtom, Message),
            {reply, Result, State}
    end;

handle_call({register_client, ClientName, RoomName}, _From, State) ->
    io:format("[server] Delegating client registration to register module: ~p in room ~p~n",
        [ClientName, RoomName]),
    %% Delegate to the register module
    Result = gen_server:call(register, {register_client, ClientName, RoomName}),
    {reply, Result, State}.

handle_cast({register_client, ClientName, RoomName}, State) ->
    io:format("[server] Delegating client registration to register module: ~p in room ~p~n",
        [ClientName, RoomName]),
    %% Delegate to the register module
    gen_server:cast(register, {register_client, ClientName, RoomName}),
    {noreply, State}.

handle_info(_Info, State) ->
    %% Handle any other messages (e.g., system messages)
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[server] Terminating~n"),
    %% Cleanup if necessary
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% Handle code changes if necessary
    {ok, State}.