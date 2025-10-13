-module(my_tracer).
-export([start/0]).

start() ->
    spawn(fun loop/0).

loop() ->
    receive
        {trace, Pid, send, Msg, To} ->
            maybe_print(Pid, io_lib:format("Process ~p sent message ~p to process ~p~n", [Pid, Msg, To])),
            loop();

        {trace, Pid, 'receive', Msg} ->
            maybe_print(Pid, io_lib:format("Process ~p received message ~p~n", [Pid, Msg])),
            loop();

        {trace, Pid, call, {Module, Function, Args}} ->
            maybe_print(Pid, io_lib:format("Function ~p:~p invoked with arguments ~p in process ~p~n", [Module, Function, Args, Pid])),
            loop();

        {trace, Pid, return_from, {Module, Function, Arity}, ReturnVal} ->
            maybe_print(Pid, io_lib:format("Function ~p:~p/~p returned ~p in process ~p~n", [Module, Function, Arity, ReturnVal, Pid])),
            loop();

        _Other ->
            loop()
    end.

maybe_print(Pid, Msg) ->
    case is_relevant(Pid) of
        true -> io:put_chars(Msg);
        false -> ok
    end.

is_relevant(Pid) ->
    case lists:keyfind(Pid, 2, get_named_processes()) of
        {Name, _} when is_atom(Name) ->
            is_target_name(Name);
        false ->
            false
    end.

%% Add proc_add10 and proc_mul2 to the list of relevant names
is_target_name(Name) ->
    lists:prefix("client", atom_to_list(Name)) orelse
    Name == main_server orelse
    Name == central_server orelse
    Name == proc_add10 orelse
    Name == proc_mul2.

get_named_processes() ->
    [{Name, whereis(Name)} || Name <- registered(), is_pid(whereis(Name))].



