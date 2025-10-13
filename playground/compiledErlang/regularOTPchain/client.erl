-module(client).
-export([start/1]).

start(Data) ->
    {ok, Reply} = gen_server:call(central, Data),
    io:format("Client got reply: ~p~n", [Reply]).
