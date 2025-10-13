
-module(central_server).
-export([start/0]).

start() ->
    Pid = spawn(fun() ->
        register(central_server, self()),
        incrementer:start(),
        loop(#{})
    end),
    Pid.

loop(State) ->
    receive
        Msg -> 
            case Msg of
                {From, Ref, {request, N}} -> 
                    incrementer ! {self(), Ref, N},
                    NewState = State#{Ref => From},
                    loop(NewState);
                {Ref, Result} -> 
                    case maps:get(Ref, State, undefined) of
                        undefined -> 
                            io:format("Unknown ref: ~p~n", [Ref]),
                            loop(State);
                        Client ->
                            Client ! {Ref, Result}, 
                            loop(maps:remove(Ref, State))
                    end
            end
    end.

