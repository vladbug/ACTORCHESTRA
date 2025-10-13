-module(context_injector).
-export([parse_transform/2]).

parse_transform(Forms, _Opts) ->
    case is_gen_server(Forms) of
        true ->
            Forms1 = ensure_context_in_state_record(Forms),
            Forms2 = inject_context_handler(Forms1),
            rewrite_process_calls(Forms2);
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

%%── Ensure the 'state' record has a 'context' field
ensure_context_in_state_record(Forms) ->
    case find_state_record(Forms) of
        {found, _RecordForm} ->
            % Check if context field already exists and modify if needed
            modify_forms_for_context(Forms);
        not_found ->
            % No state record found, create one with context field
            inject_state_record(Forms)
    end.

find_state_record(Forms) ->
    find_state_record(Forms, not_found).

find_state_record([], Acc) -> 
    Acc;
find_state_record([{attribute, _Line, record, {state, _Fields}} = Record | _], _) ->
    {found, Record};
find_state_record([_ | Rest], Acc) ->
    find_state_record(Rest, Acc).

modify_forms_for_context(Forms) ->
    lists:map(fun
        ({attribute, Line, record, {state, Fields}} = _Form) ->
            case has_context_field(Fields) of
                true ->
                    % Context field already exists, keep as is
                    {attribute, Line, record, {state, Fields}};
                false ->
                    % Add context field
                    ContextField = {record_field, Line, {atom, Line, context}, {atom, Line, undefined}},
                    NewFields = Fields ++ [ContextField],
                    {attribute, Line, record, {state, NewFields}}
            end;
        (Other) ->
            Other
    end, Forms).

has_context_field(Fields) ->
    lists:any(fun
        ({record_field, _, {atom, _, context}}) -> true;
        ({record_field, _, {atom, _, context}, _}) -> true;
        (_) -> false
    end, Fields).

inject_state_record(Forms) ->
    % Only inject if no state record exists
    case Forms of
        [{attribute, Line, module, ModName} | Rest] ->
            RecordDef = {attribute, Line, record, {state, [
                {record_field, Line, {atom, Line, context}, {atom, Line, undefined}}
            ]}},
            [{attribute, Line, module, ModName}, RecordDef | Rest];
        _ ->
            % Fallback: insert at beginning
            RecordDef = {attribute, 1, record, {state, [
                {record_field, 1, {atom, 1, context}, {atom, 1, undefined}}
            ]}},
            [RecordDef | Forms]
    end.

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
                     {record,Line,{var,Line,'State'},state,
                         [{record_field,Line,{atom,Line,context},{var,Line,'Context'}}]}
                   ]}
               ]},
            {function, Line, handle_call, 3, [CtxClause|Clauses]};
        (Other) ->
            Other
    end, Forms).

%%── 2) Rewrite calls in any clause that might contain gen_server:call
rewrite_process_calls(Forms) ->
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            NewClauses = lists:map(fun rewrite_clause/1, Clauses),
            {function, Line, handle_call, 3, NewClauses};
        (Other) ->
            Other
    end, Forms).

rewrite_clause({clause, Line, Patterns, Guards, Body}) ->
    NewBody = lists:map(fun rewrite_expression/1, Body),
    {clause, Line, Patterns, Guards, NewBody}.

%%── 3) Transform exactly gen_server:call(Target, Msg) → wrapper:call(Target, Msg, Context)
rewrite_expression({call, CLine,
                   {remote, RLine, {atom,_,gen_server}, {atom,_,call}},
                   [Target, Msg]}) ->
    % Extract context from State record
    ContextExpr = {record_field, CLine, {var, CLine, 'State'}, state, {atom, CLine, context}},
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

rewrite_expression({'if', Line, Clauses}) ->
    % Recursively rewrite if expressions
    NewClauses = lists:map(fun rewrite_case_clause/1, Clauses),
    {'if', Line, NewClauses};

rewrite_expression({'try', Line, Body, CatchClauses, AfterClause}) ->
    % Recursively rewrite try expressions
    NewBody = lists:map(fun rewrite_expression/1, Body),
    NewCatchClauses = lists:map(fun rewrite_case_clause/1, CatchClauses),
    NewAfterClause = case AfterClause of
        [] -> [];
        _ -> lists:map(fun rewrite_expression/1, AfterClause)
    end,
    {'try', Line, NewBody, NewCatchClauses, NewAfterClause};

rewrite_expression({'receive', Line, Clauses}) ->
    % Recursively rewrite receive expressions
    NewClauses = lists:map(fun rewrite_case_clause/1, Clauses),
    {'receive', Line, NewClauses};

rewrite_expression({'receive', Line, Clauses, Timeout, TimeoutBody}) ->
    % Recursively rewrite receive expressions with timeout
    NewClauses = lists:map(fun rewrite_case_clause/1, Clauses),
    NewTimeout = rewrite_expression(Timeout),
    NewTimeoutBody = lists:map(fun rewrite_expression/1, TimeoutBody),
    {'receive', Line, NewClauses, NewTimeout, NewTimeoutBody};

rewrite_expression({'fun', Line, {clauses, Clauses}}) ->
    % Recursively rewrite fun expressions
    NewClauses = lists:map(fun rewrite_case_clause/1, Clauses),
    {'fun', Line, {clauses, NewClauses}};

rewrite_expression({lc, Line, Template, Qualifiers}) ->
    % Recursively rewrite list comprehensions
    NewTemplate = rewrite_expression(Template),
    NewQualifiers = lists:map(fun rewrite_qualifier/1, Qualifiers),
    {lc, Line, NewTemplate, NewQualifiers};

rewrite_expression({bc, Line, Template, Qualifiers}) ->
    % Recursively rewrite binary comprehensions
    NewTemplate = rewrite_expression(Template),
    NewQualifiers = lists:map(fun rewrite_qualifier/1, Qualifiers),
    {bc, Line, NewTemplate, NewQualifiers};

rewrite_expression(Other) ->
    Other.

rewrite_case_clause({clause, Line, Patterns, Guards, Body}) ->
    NewBody = lists:map(fun rewrite_expression/1, Body),
    {clause, Line, Patterns, Guards, NewBody}.

rewrite_qualifier({generate, Line, Pattern, Expr}) ->
    NewExpr = rewrite_expression(Expr),
    {generate, Line, Pattern, NewExpr};
rewrite_qualifier({b_generate, Line, Pattern, Expr}) ->
    NewExpr = rewrite_expression(Expr),
    {b_generate, Line, Pattern, NewExpr};
rewrite_qualifier(Expr) when is_tuple(Expr) ->
    rewrite_expression(Expr);
rewrite_qualifier(Other) ->
    Other.