-module(context_injector).
-export([parse_transform/2]).

parse_transform(Forms, _Opts) ->
    case is_gen_server(Forms) of
        true ->
            Forms1 = inject_context_handler(Forms),
            rewrite_process_calls(Forms1);
        false ->
            Forms
    end.

%%── Check for -behaviour(gen_server)
is_gen_server(Forms) ->
    lists:any(fun
        ({attribute,_,behaviour,gen_server}) -> true;
        ({attribute,_,behavior,gen_server}) -> true;  % American spelling
        (_) -> false
    end, Forms).

%%── 1) Inject the context clause at the top of handle_call/3
inject_context_handler(Forms) ->
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            CtxClause = {clause, Line,
               [ {tuple,Line,[{atom,Line,context},{var,Line,'Context'}]},
                 {var,Line,'_From'},
                 {var,Line,'State'}
               ],
               [],
               [ {tuple,Line,
                   [ {atom,Line,reply},
                     {atom,Line,ok},
                     {call,Line,
                         {remote,Line,{atom,Line,maps},{atom,Line,put}},
                         [{atom,Line,context},{var,Line,'Context'},{var,Line,'State'}]}
                   ]}
               ]},
            {function, Line, handle_call, 3, [CtxClause|Clauses]};
        (Other) ->
            Other
    end, Forms).

%%── 2) Rewrite calls in the {process, _} clause
rewrite_process_calls(Forms) ->
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            NewClauses = lists:map(fun rewrite_clause/1, Clauses),
            {function, Line, handle_call, 3, NewClauses};
        (Other) ->
            Other
    end, Forms).

rewrite_clause({clause, Line, [Req, From, State], Guards, Body}) ->
    case Req of
        {tuple,_,[{atom,_,process},_]} ->
            NewBody = lists:map(fun rewrite_expression/1, Body),
            {clause, Line, [Req, From, State], Guards, NewBody};
        _ ->
            {clause, Line, [Req, From, State], Guards, Body}
    end.

%%── 3) Transform exactly gen_server:call(Target, Msg) → wrapper:call(Target, Msg, Context)
rewrite_expression({call, CLine,
                   {remote, RLine, {atom,_,gen_server}, {atom,_,call}},
                   [Target, Msg]}) ->
    ContextExpr = {call, CLine,
                   {remote, CLine, {atom,CLine,maps}, {atom,CLine,get}},
                   [{atom,CLine,context}, {var,CLine,'State'}, {atom,CLine,undefined}]},
    {call, CLine,
       {remote, RLine, {atom,RLine,wrapper}, {atom,RLine,call}},
       [Target, Msg, ContextExpr]};
rewrite_expression({call, CLine, Fun, Args}) ->
    % Recursively rewrite function calls in arguments
    NewArgs = lists:map(fun rewrite_expression/1, Args),
    {call, CLine, Fun, NewArgs};
rewrite_expression({tuple, Line, Elements}) ->
    % Recursively rewrite tuple elements
    NewElements = lists:map(fun rewrite_expression/1, Elements),
    {tuple, Line, NewElements};
rewrite_expression({cons, Line, Head, Tail}) ->
    % Recursively rewrite list elements
    NewHead = rewrite_expression(Head),
    NewTail = rewrite_expression(Tail),
    {cons, Line, NewHead, NewTail};
rewrite_expression({'case', Line, Expr, Clauses}) ->
    % Recursively rewrite case expressions
    NewExpr = rewrite_expression(Expr),
    NewClauses = lists:map(fun rewrite_case_clause/1, Clauses),
    {'case', Line, NewExpr, NewClauses};
rewrite_expression({match, Line, Left, Right}) ->
    % Recursively rewrite match expressions
    NewRight = rewrite_expression(Right),
    {match, Line, Left, NewRight};
rewrite_expression(Other) ->
    Other.

rewrite_case_clause({clause, Line, Patterns, Guards, Body}) ->
    NewBody = lists:map(fun rewrite_expression/1, Body),
    {clause, Line, Patterns, Guards, NewBody}.