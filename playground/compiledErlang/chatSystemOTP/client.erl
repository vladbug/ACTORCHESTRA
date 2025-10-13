% This will be the client module for the chat system
-module(client).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

%% API
-export([start_link/1, join_room/2, leave_room/2, send_message_to_room/3, get_messages/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    username,
    messages = []
}).

%%====================================================================
%% API
%%====================================================================

start_link(Username) ->
    ClientAtom = list_to_atom("client_" ++ Username),
    gen_server:start_link({local, ClientAtom}, ?MODULE, [Username], []).

join_room(Username, RoomName) ->
    ClientAtom = list_to_atom("client_" ++ Username),
    gen_server:call(ClientAtom, {join_room, RoomName}).

leave_room(Username, RoomName) ->
    ClientAtom = list_to_atom("client_" ++ Username),
    gen_server:call(ClientAtom, {leave_room, RoomName}).

send_message_to_room(Username, RoomName, Message) ->
    ClientAtom = list_to_atom("client_" ++ Username),
    gen_server:call(ClientAtom, {send_message_to_room, RoomName, Message}).

get_messages(Username) ->
    ClientAtom = list_to_atom("client_" ++ Username),
    gen_server:call(ClientAtom, get_messages).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Username]) ->
    io:format("[client] Starting for user: ~p~n", [Username]),
    {ok, #state{username = Username}}.

handle_call({join_room, RoomName}, _From, State) ->
    io:format("[client] ~p joining room ~p~n", [State#state.username, RoomName]),
    Result = server:join_room(State#state.username, RoomName),
    {reply, Result, State};

handle_call({leave_room, RoomName}, _From, State) ->
    io:format("[client] ~p leaving room ~p~n", [State#state.username, RoomName]),
    Result = server:leave_room(State#state.username, RoomName),
    {reply, Result, State};

handle_call({send_message_to_room, RoomName, Message}, _From, State) ->
    io:format("[client] ~p sending message to room ~p: ~p~n", [State#state.username, RoomName, Message]),
    Result = server:send_message_to_room(State#state.username, RoomName, Message),
    {reply, Result, State};

handle_call(get_messages, _From, State) ->
    {reply, State#state.messages, State#state{messages = []}}.

handle_cast({new_message, Sender, Message}, State) ->
    io:format("[client] Received message from ~p: ~p~n", [
        Sender, Message]),
    NewMessages = State#state.messages ++ [{Sender, Message}],
    {noreply, State#state{messages = NewMessages}}.

handle_info(_Info, State) ->
    %% Handle any other messages (e.g., system messages)
    {noreply, State}.

terminate(_Reason, State) ->
    io:format("[client] Terminating client for user: ~p~n", [State#state.username]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

