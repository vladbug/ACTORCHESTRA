
-module(incrementer).
-export([start/0]).

start() ->
    register(incrementer, spawn(fun loop/0)).

loop() ->
    receive
        {ServerPid, Ref, N} when is_pid(ServerPid), is_reference(Ref), is_integer(N) ->
            %% increment and send back
            ServerPid ! {Ref, N + 1},
            loop()
    end.

