-module(sub_monitor).
-export([start/3]).
start(Environment, Context, ParentPID) ->
    inner_loop(Environment, Context, ParentPID, #{}).
inner_loop(Environment, Context, ParentPID, ChildMap) ->
    receive
        {Src, Dst, Msg, Context} ->
            case maps:get(Context, ChildMap, undefined) of
                undefined ->
                    anchor_loop(Src, Dst, Msg, Context, Environment, ParentPID, ChildMap);
                ChildPID ->
                    ChildPID ! {Src, Dst, Msg, Context},
                    inner_loop(Environment, Context, ParentPID, ChildMap)
            end;
        {context_completed, Context, Result} ->
            case Result of
                satisfied -> ParentPID ! {context_completed, Context, satisfied};
                _ ->
                    NewChildMap = maps:remove(Context, ChildMap),
                    inner_loop(Environment, Context, ParentPID, NewChildMap)
            end;
        stop ->
            maps:fold(fun(_, PID, _) -> PID ! stop end, ok, ChildMap),
            ok
    end.
anchor_loop(Src, Dst, Msg, Context, Environment, ParentPID, ChildMap) ->
    Flag3 = false,
    case {Src, Dst, Msg} of
        {b, c, {Z}} ->
            Constraint = (Z == maps:get(x, Environment)),
            if Constraint ->
                NewEnv = maps:merge(Environment, #{z => Z}),
                ChildPID = spawn(sub_monitor_1, start, [NewEnv, Context, self()]),
                NewChildMap = ChildMap#{Context => ChildPID},
                inner_loop(Environment, Context, ParentPID, NewChildMap)
            ; true ->
                Flag3 = true,
                inner_loop(Environment, Context, ParentPID, ChildMap)
            end;
        _ ->
            Flag3 = true,
            inner_loop(Environment, Context, ParentPID, ChildMap)
    end.
