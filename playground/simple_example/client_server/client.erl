
-module(client).
-export([start/1]).

start(Number) ->
    spawn(fun() -> loop(Number) end).

loop(Number) ->
    Ref = make_ref(),
    central_server ! {self(), Ref, {request, Number}},
    receive
        {_, Ref, {response, Result}} ->
            io:format("Client ~p: Got response ~p~n", [self(), Result])
    after 2000 ->
        io:format("Client ~p: Timeout waiting for response~n", [self()])
    end.

