-module(my_tracer).
-export([start/0]).

start() ->
    spawn(fun init/0).

init() ->
    {ok, File} = file:open("trace.log", [write, {encoding, utf8}]),
    loop(File).

loop(File) ->
    receive
        {trace, Pid, call, {M, F, A}} ->
            io:format(File, "[CALL] ~p called ~p:~p/~p~n", [Pid, M, F, length(A)]),
            loop(File);

        {trace, Pid, return_from, {M, F, Arity}, ReturnVal} ->
            io:format(File, "[RET ] ~p returned from ~p:~p/~p with ~p~n", [Pid, M, F, Arity, ReturnVal]),
            loop(File);

        {trace, Pid, send, Msg, To} ->
            io:format(File, "[SEND] ~p -> ~p :: ~p~n", [Pid, To, Msg]),
            loop(File);

        {trace, Pid, 'receive', Msg} ->
            io:format(File, "[RECV] ~p received :: ~p~n", [Pid, Msg]),
            loop(File);

        {stop} ->
            io:format(File, "Tracer stopping~n", []),
            file:close(File),
            ok;

        Other ->
            io:format(File, "[OTHER] ~p~n", [Other]),
            loop(File)
    end.
