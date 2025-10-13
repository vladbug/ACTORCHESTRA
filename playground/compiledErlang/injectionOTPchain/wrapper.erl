-module(wrapper).
-behaviour(gen_server).

-export([start_link/0, call/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

%% Public API
-spec call(pid() | atom(), term(), reference() | undefined) -> any().
call(To, Msg, undefined) ->
    Context = make_ref(),
    io:format("Calling ~p with message ~p and context ~p~n", [To, Msg, Context]),
    gen_server:call(?MODULE, {send_with_context, To, Msg, Context});
call(To, Msg, Context) ->
    io:format("Calling ~p with message ~p and context ~p~n", [To, Msg, Context]),
    gen_server:call(?MODULE, {send_with_context, To, Msg, Context}).

%% Break the deadlock by spawning a helper
handle_call({send_with_context, To, Msg, Context}, From, State) ->
    spawn_link(fun() ->
        io:format("[wrapper] sending ~p to ~p with context ~p~n", [Msg, To, Context]),
        ok = gen_server:call(To, {context, Context}),
        Reply = gen_server:call(To, Msg),
        io:format("[wrapper] received ~p from ~p with context ~p~n", [Reply, To, Context]),
        gen_server:reply(From, Reply)
    end),
    {noreply, State};

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
