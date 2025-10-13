-module(baseline_client).
-behaviour(gen_server).

%% Only gen_server functions exported, no API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% gen_server startup only

start_link(ClientName) ->
    gen_server:start_link({local, ClientName}, ?MODULE, [ClientName], []).

%%% gen_server callbacks

init([ClientName]) ->
    {ok, #{client_name => ClientName}}.

%% Raw handle_calls - client just forwards everything to chat_server (like add/mult system)

handle_call(register, _From, State) ->
    ClientName = maps:get(client_name, State),
    %% Just forward to chat_server - no state management here
    Result = gen_server:call(baseline_chat_server, {register_client, ClientName, self()}),
    {reply, Result, State};

handle_call({join_room, RoomName}, _From, State) ->
    ClientName = maps:get(client_name, State),
    %% Just forward to chat_server - no state management here
    Result = gen_server:call(baseline_chat_server, {join_room, ClientName, RoomName}),
    {reply, Result, State};

handle_call({send_message, RoomName, Message}, _From, State) ->
    ClientName = maps:get(client_name, State),
    %% Just forward to chat_server - no state management here
    Result = gen_server:call(baseline_chat_server, {send_message, ClientName, RoomName, Message}),
    io:format("[~p] Sent message to ~p: ~s~n", [ClientName, RoomName, Message]),
    {reply, Result, State};

handle_call({leave_room, RoomName}, _From, State) ->
    ClientName = maps:get(client_name, State),
    %% Just forward to chat_server - no state management here
    Result = gen_server:call(baseline_chat_server, {leave_room, ClientName, RoomName}),
    {reply, Result, State};

handle_call(get_status, _From, State) ->
    ClientName = maps:get(client_name, State),
    %% Forward to chat_server for status
    Result = gen_server:call(baseline_chat_server, {get_client_status, ClientName}),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Handle incoming messages from rooms (via gen_server:call from room)
handle_cast({receive_message, RoomName, FromClient, Message}, State) ->
    ClientName = maps:get(client_name, State),
    io:format("[~p] Message in ~p from ~p: ~s~n", [ClientName, RoomName, FromClient, Message]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.