% This process will be a room in the chat system
-module(room).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

%% API
-export([start_link/1, join_room/2, leave_room/2, send_message/3, receive_messages/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    users = [],
    messages = []
}).

%%====================================================================
%% API
%%====================================================================
start_link(RoomName) ->
    gen_server:start_link({local, RoomName}, ?MODULE, [], []).
join_room(User, RoomName) ->
    gen_server:call(RoomName, {join, User}).

leave_room(User, RoomName) ->
    gen_server:call(RoomName, {leave, User}).

send_message(User, RoomName, Message) ->
    gen_server:call(RoomName, {send_message, User, Message}).

receive_messages(RoomName) ->
    gen_server:call(RoomName, receive_messages).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[room] Starting~n"),
    {ok, #state{}}.

handle_call({join, User}, _From, State) ->
    io:format("[room] User ~p joined~n", [User]),
    {reply, ok, State#state{users = [User | State#state.users]}};

handle_call({leave, User}, _From, State) ->
    io:format("[room] User ~p left~n", [User]),
    NewUsers = lists:filter(fun(U) -> U =/= User end, State#state.users),
    {reply, ok, State#state{users = NewUsers}};

handle_call({send_message, User, Message}, _From, State) ->
    io:format("[room] User ~p sent message: ~p~n", [User, Message]),
    NewMessages = State#state.messages ++ [{User, Message}],
    %% Broadcast message to all users in the room
    lists:foreach(fun(Username) ->
        ClientAtom = list_to_atom("client_" ++ Username),
        case whereis(ClientAtom) of
            undefined ->
                io:format("[room] Client ~p not found, skipping broadcast~n", [Username]);
            _ClientPid ->
                gen_server:cast(ClientAtom, {new_message, User, Message})
        end
    end, State#state.users),
    {reply, ok, State#state{messages = NewMessages}};

handle_call(receive_messages, _From, State) ->
    io:format("[room] Retrieving messages~n"),
    {reply, State#state.messages, State#state{messages = []}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    %% Handle any other messages (e.g., system messages)
    {noreply, State}.   

terminate(_Reason, State) ->
    io:format("[room] Terminating room~n"),
    {stop, normal, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.