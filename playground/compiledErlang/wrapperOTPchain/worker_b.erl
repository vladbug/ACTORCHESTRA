-module(worker_b).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    context = undefined
}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({context, Ctx}, _From, State) ->
    {reply, ok, State#state{context = Ctx}};

handle_call({process, N}, _, State = #state{context = _Ctx}) ->
    Result = N * 2,
    %gen_server:reply(From, Result),
    {reply, Result, State};

handle_call(_, _From, State) ->
    {reply, error, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
