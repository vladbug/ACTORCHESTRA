-module(central).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, { context = undefined }).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

%% Primary entry point — no context yet
handle_call({process, N}, _ , State = #state{context = Ctx}) ->
    Reply = wrapper:call(worker_a, {process, N}, Ctx),
    {reply, Reply, State};

%% Context arrives
handle_call({context, Ctx}, _From, State) ->
    {reply, ok, State#state{context = Ctx}};

handle_call(_, _From, State) ->
    {reply, {error, bad_request}, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_R, _S) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
