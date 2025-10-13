-module(my_tracer).
-export([start/0]).

start() ->
  %spawn(?MODULE, loop, []).
  spawn(fun loop/0).

loop() ->
  receive
    {trace, Pid, send, Msg, To} ->
      io:format("Process ~p sent message ~p to process ~p~n", [Pid, Msg, To]),
      loop();
    {trace, Pid, 'receive', Msg} ->
      io:format("Process ~p received message ~p~n", [Pid, Msg]),
      loop();
    {trace, Pid, return_from, {Module, Function, Arity}, ReturnValue} ->
      io:format("Function ~p:~p/~p returned ~p in process ~p~n", [Module, Function, Arity, ReturnValue, Pid]),
      loop();
    {trace, Pid, call, {Module, Function, Arguments}} ->
      io:format("Function ~p:~p invoked with arguments ~p in process ~p~n", [Module, Function, Arguments, Pid]),
      loop();
    Msg ->
      io:format("Received an unexpected message: ~p~n", [Msg]),
      loop()
  end.
