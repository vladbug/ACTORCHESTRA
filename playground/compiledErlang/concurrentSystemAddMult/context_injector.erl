-module(context_injector).
-export([parse_transform/2]).

parse_transform(Forms, _Opts) ->
    case is_gen_server(Forms) of
        true ->
            case detect_state_approach(Forms) of
                record ->
                    % Use record-based approach
                    Forms1 = ensure_context_in_state_record(Forms),
                    Forms2 = inject_context_handler_record(Forms1),
                    rewrite_process_calls_record(Forms2);
                map ->
                    % Use map-based approach
                    Forms1 = inject_context_handler_map(Forms),
                    rewrite_process_calls_map(Forms1)
            end;
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

%%── Detect whether to use record or map approach
detect_state_approach(Forms) ->
    case find_state_record(Forms) of
        {found, _} -> record;
        not_found -> map
    end.

%%────────────────────────────────────────────────────────────────
%%── RECORD-BASED APPROACH
%%────────────────────────────────────────────────────────────────

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
    inject_state_record(Forms, []).

inject_state_record([], Acc) ->
    % No module found, just append record at end
    RecordDef = {attribute, 1, record, {state, [
        {record_field, 1, {atom, 1, context}, {atom, 1, undefined}}
    ]}},
    lists:reverse([RecordDef | Acc]);

inject_state_record([{attribute, Line, module, _ModName} = ModAttr | Rest], Acc) ->
    % Found module, insert record after it
    RecordDef = {attribute, Line, record, {state, [
        {record_field, Line, {atom, Line, context}, {atom, Line, undefined}}
    ]}},
    lists:reverse(Acc) ++ [ModAttr, RecordDef | Rest];

inject_state_record([Form | Rest], Acc) ->
    inject_state_record(Rest, [Form | Acc]).

%%── Inject context handler for record-based approach
inject_context_handler_record(Forms) ->
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

%%── Rewrite calls for record-based approach
rewrite_process_calls_record(Forms) ->
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            NewClauses = lists:map(fun rewrite_clause_record/1, Clauses),
            {function, Line, handle_call, 3, NewClauses};
        (Other) ->
            Other
    end, Forms).

rewrite_clause_record({clause, Line, Patterns, Guards, Body}) ->
    NewBody = lists:map(fun rewrite_expression_record/1, Body),
    {clause, Line, Patterns, Guards, NewBody}.

%%── Transform gen_server:call for record approach
rewrite_expression_record({call, CLine,
                   {remote, RLine, {atom,_,gen_server}, {atom,_,call}},
                   [Target, Msg]}) ->
    % Extract context from State record
    ContextExpr = {record_field, CLine, {var, CLine, 'State'}, state, {atom, CLine, context}},
    {call, CLine,
       {remote, RLine, {atom,RLine,wrapper}, {atom,RLine,call}},
       [Target, Msg, ContextExpr]};

rewrite_expression_record({call, CLine, Fun, Args}) ->
    NewArgs = lists:map(fun rewrite_expression_record/1, Args),
    {call, CLine, Fun, NewArgs};

rewrite_expression_record({tuple, Line, Elements}) ->
    NewElements = lists:map(fun rewrite_expression_record/1, Elements),
    {tuple, Line, NewElements};

rewrite_expression_record({cons, Line, Head, Tail}) ->
    NewHead = rewrite_expression_record(Head),
    NewTail = rewrite_expression_record(Tail),
    {cons, Line, NewHead, NewTail};

rewrite_expression_record({'case', Line, Expr, Clauses}) ->
    NewExpr = rewrite_expression_record(Expr),
    NewClauses = lists:map(fun rewrite_case_clause_record/1, Clauses),
    {'case', Line, NewExpr, NewClauses};

rewrite_expression_record({match, Line, Left, Right}) ->
    NewRight = rewrite_expression_record(Right),
    {match, Line, Left, NewRight};

rewrite_expression_record({'if', Line, Clauses}) ->
    NewClauses = lists:map(fun rewrite_case_clause_record/1, Clauses),
    {'if', Line, NewClauses};

rewrite_expression_record({'try', Line, Body, CatchClauses, AfterClause}) ->
    NewBody = lists:map(fun rewrite_expression_record/1, Body),
    NewCatchClauses = lists:map(fun rewrite_case_clause_record/1, CatchClauses),
    NewAfterClause = case AfterClause of
        [] -> [];
        _ -> lists:map(fun rewrite_expression_record/1, AfterClause)
    end,
    {'try', Line, NewBody, NewCatchClauses, NewAfterClause};

rewrite_expression_record({'receive', Line, Clauses}) ->
    NewClauses = lists:map(fun rewrite_case_clause_record/1, Clauses),
    {'receive', Line, NewClauses};

rewrite_expression_record({'receive', Line, Clauses, Timeout, TimeoutBody}) ->
    NewClauses = lists:map(fun rewrite_case_clause_record/1, Clauses),
    NewTimeout = rewrite_expression_record(Timeout),
    NewTimeoutBody = lists:map(fun rewrite_expression_record/1, TimeoutBody),
    {'receive', Line, NewClauses, NewTimeout, NewTimeoutBody};

rewrite_expression_record({'fun', Line, {clauses, Clauses}}) ->
    NewClauses = lists:map(fun rewrite_case_clause_record/1, Clauses),
    {'fun', Line, {clauses, NewClauses}};

rewrite_expression_record({lc, Line, Template, Qualifiers}) ->
    NewTemplate = rewrite_expression_record(Template),
    NewQualifiers = lists:map(fun rewrite_qualifier_record/1, Qualifiers),
    {lc, Line, NewTemplate, NewQualifiers};

rewrite_expression_record({bc, Line, Template, Qualifiers}) ->
    NewTemplate = rewrite_expression_record(Template),
    NewQualifiers = lists:map(fun rewrite_qualifier_record/1, Qualifiers),
    {bc, Line, NewTemplate, NewQualifiers};

rewrite_expression_record(Other) ->
    Other.

rewrite_case_clause_record({clause, Line, Patterns, Guards, Body}) ->
    NewBody = lists:map(fun rewrite_expression_record/1, Body),
    {clause, Line, Patterns, Guards, NewBody}.

rewrite_qualifier_record({generate, Line, Pattern, Expr}) ->
    NewExpr = rewrite_expression_record(Expr),
    {generate, Line, Pattern, NewExpr};
rewrite_qualifier_record({b_generate, Line, Pattern, Expr}) ->
    NewExpr = rewrite_expression_record(Expr),
    {b_generate, Line, Pattern, NewExpr};
rewrite_qualifier_record(Expr) when is_tuple(Expr) ->
    rewrite_expression_record(Expr);
rewrite_qualifier_record(Other) ->
    Other.

%%────────────────────────────────────────────────────────────────
%%── MAP-BASED APPROACH
%%────────────────────────────────────────────────────────────────

%%── Inject context handler for map-based approach
inject_context_handler_map(Forms) ->
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

%%── Rewrite calls for map-based approach
rewrite_process_calls_map(Forms) ->
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            NewClauses = lists:map(fun rewrite_clause_map/1, Clauses),
            {function, Line, handle_call, 3, NewClauses};
        (Other) ->
            Other
    end, Forms).

rewrite_clause_map({clause, Line, Patterns, Guards, Body}) ->
    NewBody = lists:map(fun rewrite_expression_map/1, Body),
    {clause, Line, Patterns, Guards, NewBody}.

%%── Transform gen_server:call for map approach
rewrite_expression_map({call, CLine,
                   {remote, RLine, {atom,_,gen_server}, {atom,_,call}},
                   [Target, Msg]}) ->
    ContextExpr = {call, CLine,
                   {remote, CLine, {atom,CLine,maps}, {atom,CLine,get}},
                   [{atom,CLine,context}, {var,CLine,'State'}, {atom,CLine,undefined}]},
    {call, CLine,
       {remote, RLine, {atom,RLine,wrapper}, {atom,RLine,call}},
       [Target, Msg, ContextExpr]};

rewrite_expression_map({call, CLine, Fun, Args}) ->
    NewArgs = lists:map(fun rewrite_expression_map/1, Args),
    {call, CLine, Fun, NewArgs};

rewrite_expression_map({tuple, Line, Elements}) ->
    NewElements = lists:map(fun rewrite_expression_map/1, Elements),
    {tuple, Line, NewElements};

rewrite_expression_map({cons, Line, Head, Tail}) ->
    NewHead = rewrite_expression_map(Head),
    NewTail = rewrite_expression_map(Tail),
    {cons, Line, NewHead, NewTail};

rewrite_expression_map({'case', Line, Expr, Clauses}) ->
    NewExpr = rewrite_expression_map(Expr),
    NewClauses = lists:map(fun rewrite_case_clause_map/1, Clauses),
    {'case', Line, NewExpr, NewClauses};

rewrite_expression_map({match, Line, Left, Right}) ->
    NewRight = rewrite_expression_map(Right),
    {match, Line, Left, NewRight};

rewrite_expression_map({'if', Line, Clauses}) ->
    NewClauses = lists:map(fun rewrite_case_clause_map/1, Clauses),
    {'if', Line, NewClauses};

rewrite_expression_map({'try', Line, Body, CatchClauses, AfterClause}) ->
    NewBody = lists:map(fun rewrite_expression_map/1, Body),
    NewCatchClauses = lists:map(fun rewrite_case_clause_map/1, CatchClauses),
    NewAfterClause = case AfterClause of
        [] -> [];
        _ -> lists:map(fun rewrite_expression_map/1, AfterClause)
    end,
    {'try', Line, NewBody, NewCatchClauses, NewAfterClause};

rewrite_expression_map({'receive', Line, Clauses}) ->
    NewClauses = lists:map(fun rewrite_case_clause_map/1, Clauses),
    {'receive', Line, NewClauses};

rewrite_expression_map({'receive', Line, Clauses, Timeout, TimeoutBody}) ->
    NewClauses = lists:map(fun rewrite_case_clause_map/1, Clauses),
    NewTimeout = rewrite_expression_map(Timeout),
    NewTimeoutBody = lists:map(fun rewrite_expression_map/1, TimeoutBody),
    {'receive', Line, NewClauses, NewTimeout, NewTimeoutBody};

rewrite_expression_map({'fun', Line, {clauses, Clauses}}) ->
    NewClauses = lists:map(fun rewrite_case_clause_map/1, Clauses),
    {'fun', Line, {clauses, NewClauses}};

rewrite_expression_map({lc, Line, Template, Qualifiers}) ->
    NewTemplate = rewrite_expression_map(Template),
    NewQualifiers = lists:map(fun rewrite_qualifier_map/1, Qualifiers),
    {lc, Line, NewTemplate, NewQualifiers};

rewrite_expression_map({bc, Line, Template, Qualifiers}) ->
    NewTemplate = rewrite_expression_map(Template),
    NewQualifiers = lists:map(fun rewrite_qualifier_map/1, Qualifiers),
    {bc, Line, NewTemplate, NewQualifiers};

rewrite_expression_map(Other) ->
    Other.

rewrite_case_clause_map({clause, Line, Patterns, Guards, Body}) ->
    NewBody = lists:map(fun rewrite_expression_map/1, Body),
    {clause, Line, Patterns, Guards, NewBody}.

rewrite_qualifier_map({generate, Line, Pattern, Expr}) ->
    NewExpr = rewrite_expression_map(Expr),
    {generate, Line, Pattern, NewExpr};
rewrite_qualifier_map({b_generate, Line, Pattern, Expr}) ->
    NewExpr = rewrite_expression_map(Expr),
    {b_generate, Line, Pattern, NewExpr};
rewrite_qualifier_map(Expr) when is_tuple(Expr) ->
    rewrite_expression_map(Expr);
rewrite_qualifier_map(Other) ->
    Other.