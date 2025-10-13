-module(waltz_monitor).
-export([start/0, stop/0, monitor_loop/0]).

start() ->
    case whereis(waltz_monitor) of
        undefined ->
            Pid = spawn(?MODULE, monitor_loop, []),
            register(waltz_monitor, Pid),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

stop() ->
    case whereis(waltz_monitor) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop,
            unregister(waltz_monitor),
            ok
    end.

monitor_loop() ->
    monitor_loop(#{}).

monitor_loop(ContextMap) ->
    receive
        stop ->
            maps:fold(fun(_, PID, _) -> PID ! stop end, ok, ContextMap),
            ok;
        {Src, Dst, Msg, Context} ->
            case maps:get(Context, ContextMap, undefined) of
                undefined ->
                    mainLoop5(Src, Dst, Msg, Context, ContextMap);
                PID ->
                    PID ! {Src, Dst, Msg, Context},
                    monitor_loop(ContextMap)
            end;
        {context_completed, Context, Result} ->
            io:format("Context ~p completed: ~p~n", [Context, Result]),
            NewContextMap = maps:remove(Context, ContextMap),
            case Result of
                satisfied -> satisfied;
                _ -> monitor_loop(NewContextMap)
            end
    end.

mainLoop5(Src, Dst, Msg, Context, ContextMap) ->
    Flag4 = false,
    case {Src, Dst, Msg} of
        {a, b, {X, Y, add}} ->
            Constraint = true,
            if Constraint ->
                Environment = #{x => X, y => Y},
                SubPID = spawn(sub_monitor, start, [Environment, Context, self()]),
                NewContextMap = ContextMap#{Context => SubPID},
                monitor_loop(NewContextMap)
            ; true ->
                Flag4 = true,
                monitor_loop(ContextMap)
            end;
        _ ->
            Flag4 = true,
            monitor_loop(ContextMap)
    end.
