-module(sub_monitor_1).
-export([start/3]).
start(Environment, Context, ParentPID) ->
    inner_loop(Environment, Context, ParentPID).
inner_loop(Environment, Context, ParentPID) ->
    Flag2 = false,
    receive
        {a, c, {I}, Context} ->
            Constraint = (I > 0),
            if Constraint ->
                ParentPID ! {context_completed, Context, satisfied}
            ; true ->
                Flag2 = true
            end;
        stop ->
            ok
    end,
    if Flag2 ->
        inner_loop(Environment, Context, ParentPID)
    ; true ->
        ParentPID ! {context_completed, Context, satisfied}
    end.
