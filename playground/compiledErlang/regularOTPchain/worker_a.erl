-module(worker_a).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, worker_a}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call({process, N}, _From, State) ->
    io:format("worker_a received: ~p~n", [N]),
    N1 = N + 10,
    io:format("worker_a sends: ~p to worker_b~n", [N1]),
    Result = gen_server:call(worker_b, {process, N1}),
    {reply, Result, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
