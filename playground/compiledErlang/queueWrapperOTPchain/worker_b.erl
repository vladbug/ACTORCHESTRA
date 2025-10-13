-compile({parse_transform, context_injector}).
-module(worker_b).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, worker_b}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.
    
handle_call({process, N}, _From, State) ->
    io:format("worker_b received: ~p~n", [N]),
    N2 = N * 2,
    io:format("worker_b result: ~p~n", [N2]),
    {reply, N2, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
