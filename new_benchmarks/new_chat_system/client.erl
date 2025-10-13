-module(client).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

%% Only gen_server functions exported, no API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% gen_server startup only

start_link(ClientName) ->
    gen_server:start_link({local, ClientName}, ?MODULE, [ClientName], []).

%%% gen_server callbacks

init([ClientName]) ->
    {ok, #{client_name => ClientName}}.


handle_call(register, _From, State) ->
    Context = maps:get(context, State, undefined),
    ClientName = maps:get(client_name, State),
    %% Just forward to chat_server - no state management here
    {Result, NewContext} = simple_wrapper:call(chat_server, {register_client, ClientName, self()}, Context, client),
    {reply, Result, State#{context => NewContext}};

handle_call({join_room, RoomName}, _From, State) ->
    Context = maps:get(context, State, undefined),
    ClientName = maps:get(client_name, State),
    %% Just forward to chat_server - no state management here
    {Result, NewContext} = simple_wrapper:call(chat_server, {join_room, ClientName, RoomName}, Context, client),
    {reply, Result, State#{context => NewContext}};

handle_call({send_message, RoomName, Message}, _From, State) ->
    Context = maps:get(context, State, undefined),
    ClientName = maps:get(client_name, State),
    %% Just forward to chat_server - no state management here
    {Result, NewContext} = simple_wrapper:call(chat_server, {send_message, ClientName, RoomName, Message}, Context, client),
    io:format("[~p] Sent message to ~p: ~s~n", [ClientName, RoomName, Message]),
    {reply, Result, State#{context => NewContext}};

handle_call({leave_room, RoomName}, _From, State) ->
    Context = maps:get(context, State, undefined),
    ClientName = maps:get(client_name, State),
    %% Just forward to chat_server - no state management here
    {Result, NewContext} = simple_wrapper:call(chat_server, {leave_room, ClientName, RoomName}, Context, client),
    {reply, Result, State#{context => NewContext}};

handle_call(get_status, _From, State) ->
    Context = maps:get(context, State, undefined),
    ClientName = maps:get(client_name, State),
    %% Forward to chat_server for status
    {Result, NewContext} = simple_wrapper:call(chat_server, {get_client_status, ClientName}, Context, client),
    {reply, Result, State#{context => NewContext}};

handle_call(get_name_client, _From, State) ->
    ClientName = maps:get(client_name, State),
    {reply, {ok, ClientName}, State};

%% Handle incoming messages from rooms (via gen_server:call from room)
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

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