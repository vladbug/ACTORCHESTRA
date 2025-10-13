% The server.erl will communicate with this register module to register clietns into chat rooms
-module(register).
-behaviour(gen_server).
-export([start_link/0, get_clients_in_room/1, get_all_clients/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile({parse_transform, context_injector}).

-record(state, {
    rooms = [],
    clients = []  % List of {ClientName, RoomName} tuples
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_clients_in_room(RoomName) ->
    gen_server:call(?MODULE, {get_clients_in_room, RoomName}).

get_all_clients() ->
    gen_server:call(?MODULE, {get_all_clients}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[register] Starting~n"),
    {ok, #state{}}.

handle_call({register_room, RoomName}, _From, State) ->
    io:format("[register] Registering room: ~p~n", [RoomName]),
    NewRooms = [RoomName | State#state.rooms],
    {reply, ok, State#state{rooms = NewRooms}};

handle_call({get_rooms}, _From, State) ->
    io:format("[register] Retrieving registered rooms~n"),
    {reply, State#state.rooms, State};

handle_call({get_clients_in_room, RoomName}, _From, State) ->
    io:format("[register] Retrieving clients in room ~p~n", [RoomName]),
    Clients = [ClientName || {ClientName, Room} <- State#state.clients, Room =:= RoomName],
    {reply, Clients, State};

handle_call({get_all_clients}, _From, State) ->
    io:format("[register] Retrieving all registered clients~n"),
    {reply, State#state.clients, State};

handle_call({register_client, ClientName, RoomName}, _From, State) ->
    io:format("[register] Registering client ~p in room ~p~n",
        [ClientName, RoomName]),
    %% Check if room exists
    case lists:member(RoomName, State#state.rooms) of
        true ->
            %% Add client to the room
            NewClients = [{ClientName, RoomName} | State#state.clients],
            io:format("[register] Client ~p successfully registered in room ~p~n", 
                [ClientName, RoomName]),
            {reply, ok, State#state{clients = NewClients}};
        false ->
            io:format("[register] Room ~p does not exist, cannot register client ~p~n", 
                [RoomName, ClientName]),
            {reply, {error, room_not_found}, State}
    end.

handle_cast({register_client, ClientName, RoomName}, State) ->
    io:format("[register] Registering client ~p in room ~p~n",
        [ClientName, RoomName]),
    %% Check if room exists
    case lists:member(RoomName, State#state.rooms) of
        true ->
            %% Add client to the room
            NewClients = [{ClientName, RoomName} | State#state.clients],
            io:format("[register] Client ~p successfully registered in room ~p~n", 
                [ClientName, RoomName]),
            {noreply, State#state{clients = NewClients}};
        false ->
            io:format("[register] Room ~p does not exist, cannot register client ~p~n", 
                [RoomName, ClientName]),
            {noreply, State}
    end.

handle_info(_Info, State) ->
    %% Handle any other messages (e.g., system messages)
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[register] Terminating~n"),
    %% Cleanup if necessary
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% Handle code changes if necessary
    {ok, State}.

