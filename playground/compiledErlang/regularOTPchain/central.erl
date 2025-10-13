-module(central).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([send_number/1]).

start_link() ->
    gen_server:start_link({local, central}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

send_number(Number) ->
    gen_server:call(central, {number, Number}).

handle_call({number, N}, _From, State) ->
    io:format("central received number: ~p~n", [N]),
    Result = gen_server:call(worker_a, {process, N}),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
