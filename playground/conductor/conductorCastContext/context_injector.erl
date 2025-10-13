-module(context_injector).
-export([parse_transform/2]).

parse_transform(Forms, _Opts) ->
    case is_gen_server(Forms) of
        true ->
            ModuleName = extract_module_name(Forms),
            io:format("PARSE_TRANSFORM: Processing gen_server module ~p~n", [ModuleName]),
            case detect_state_approach(Forms) of
                record ->
                    io:format("PARSE_TRANSFORM: Using record-based approach for ~p~n", [ModuleName]),
                    Forms1 = ensure_context_in_state_record(Forms),
                    Forms2 = inject_with_context_handler_record(Forms1),
                    Forms3 = inject_context_cast_handler_record(Forms2),
                    rewrite_process_calls_record(Forms3, ModuleName);
                map ->
                    io:format("PARSE_TRANSFORM: Using map-based approach for ~p~n", [ModuleName]),
                    Forms1 = inject_with_context_handler_map(Forms),
                    Forms2 = inject_context_cast_handler_map(Forms1),
                    rewrite_process_calls_map(Forms2, ModuleName)
            end;
        false ->
            io:format("PARSE_TRANSFORM: Skipping non-gen_server module~n"),
            Forms
    end.

%%── Extract module name from Forms
extract_module_name(Forms) ->
    extract_module_name(Forms, undefined).

extract_module_name([], ModuleName) ->
    ModuleName;
extract_module_name([{attribute, _, module, ModuleName} | _], _) ->
    ModuleName;
extract_module_name([_ | Rest], Acc) ->
    extract_module_name(Rest, Acc).

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
        {found, _} -> 
            io:format("PARSE_TRANSFORM: Found state record, using record approach~n"),
            record;
        not_found -> 
            io:format("PARSE_TRANSFORM: No state record found, using map approach~n"),
            map
    end.

%%────────────────────────────────────────────────────────────────────────────
%%── RECORD-BASED APPROACH
%%────────────────────────────────────────────────────────────────────────────

%%── Ensure the 'state' record has a 'current_context' field
ensure_context_in_state_record(Forms) ->
    case find_state_record(Forms) of
        {found, _RecordForm} ->
            io:format("PARSE_TRANSFORM: Found existing state record, checking for current_context field~n"),
            modify_forms_for_context(Forms);
        not_found ->
            io:format("PARSE_TRANSFORM: No state record found, injecting new one~n"),
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
                    io:format("PARSE_TRANSFORM: context field already exists in state record~n"),
                    {attribute, Line, record, {state, Fields}};
                false ->
                    io:format("PARSE_TRANSFORM: Adding context field to existing state record~n"),
                    ContextField = {record_field, Line, {atom, Line, context}, {atom, Line, undefined}},
                    NewFields = Fields ++ [ContextField],
                    io:format("PARSE_TRANSFORM: Record now has ~p fields~n", [length(NewFields)]),
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

%%── Inject with_context handler for record-based approach (handle_call only)
inject_with_context_handler_record(Forms) ->
    ModuleName = extract_module_name(Forms),
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            io:format("PARSE_TRANSFORM: Injecting with_context handler into handle_call/3 (record)~n"),
            
            %% Only the with_context handler - NO context call handler
            WithContextClause = {clause, Line,
               [ {tuple,Line,[{atom,Line,with_context},{var,Line,'Context'},{var,Line,'Msg'}]},
                 {var,Line,'From'},
                 {var,Line,'State'}
               ],
               [],
               [ {match,Line,{var,Line,'ReceiveTime'},
                   {call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,system_time}},
                       [{atom,Line,millisecond}]}},
                 {call,Line,{remote,Line,{atom,Line,io},{atom,Line,format}},
                     [{string,Line,"[~p ms] [~p] RECEIVED with_context: ctx=~p, msg=~p~n"},
                      {cons,Line,{var,Line,'ReceiveTime'},
                          {cons,Line,{atom,Line,ModuleName},
                              {cons,Line,{var,Line,'Context'},
                                  {cons,Line,{var,Line,'Msg'},{nil,Line}}}}}]},
                 {match,Line,{var,Line,'StateWithContext'},
                     {record,Line,{var,Line,'State'},state,
                         [{record_field,Line,{atom,Line,context},{var,Line,'Context'}}]}},
                 {call,Line,{remote,Line,{atom,Line,io},{atom,Line,format}},
                     [{string,Line,"[~p ms] [~p] Context ~p injected, delegating message ~p~n"},
                      {cons,Line,{var,Line,'ReceiveTime'},
                          {cons,Line,{atom,Line,ModuleName},
                              {cons,Line,{var,Line,'Context'},
                                  {cons,Line,{var,Line,'Msg'},{nil,Line}}}}}]},
                 %% Direct call to the module's handle_call function
                 {call,Line,{remote,Line,{atom,Line,ModuleName},{atom,Line,handle_call}},
                     [{var,Line,'Msg'},{var,Line,'From'},{var,Line,'StateWithContext'}]}
               ]},

            {function, Line, handle_call, 3, [WithContextClause | Clauses]};
        (Other) ->
            Other
    end, Forms).

%%── Inject context handler for record-based approach (handle_cast only)
inject_context_cast_handler_record(Forms) ->
    lists:map(fun
        ({function, Line, handle_cast, 2, Clauses}) ->
            io:format("PARSE_TRANSFORM: Injecting context handler into handle_cast/2 (record)~n"),
            
            %% Context cast handler
            ContextCastClause = {clause, Line,
               [ {tuple,Line,[{atom,Line,context},{var,Line,'Context'}]},
                 {var,Line,'State'}
               ],
               [],
               [ {tuple,Line,
                   [ {atom,Line,noreply},
                     {record,Line,{var,Line,'State'},state,
                         [{record_field,Line,{atom,Line,context},{var,Line,'Context'}}]}
                   ]}
               ]},

            {function, Line, handle_cast, 2, [ContextCastClause | Clauses]};
        (Other) ->
            Other
    end, Forms).

%%── Rewrite calls for record-based approach WITH context extraction at header
rewrite_process_calls_record(Forms, ModuleName) ->
    io:format("PARSE_TRANSFORM: Starting rewrite_process_calls_record for ~p~n", [ModuleName]),
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            %% Separate injected clauses from original clauses
            {InjectedClauses, OriginalClauses} = separate_clauses(Clauses),
            
            %% Only rewrite original clauses (not injected ones)
            NewOriginalClauses = lists:map(fun(Clause) -> rewrite_clause_record(Clause, ModuleName) end, OriginalClauses),
            
            %% Combine: injected clauses first (unchanged), then rewritten original clauses
            NewClauses = InjectedClauses ++ NewOriginalClauses,
            {function, Line, handle_call, 3, NewClauses};
        (Other) ->
            Other
    end, Forms).

%% Separate injected handler clauses from original user clauses
separate_clauses(Clauses) ->
    lists:partition(fun(Clause) -> is_injected_clause(Clause) end, Clauses).

%% Detect if a clause is an injected handler (updated to only detect with_context in handle_call)
is_injected_clause({clause, _, [{tuple, _, [{atom, _, with_context}, _, _]}, _, _], _, _}) ->
    true;
is_injected_clause(_) ->
    false.

rewrite_clause_record({clause, Line, Patterns, Guards, Body}, ModuleName) ->
    io:format("PARSE_TRANSFORM: Rewriting original user clause with ~p expressions in body~n", [length(Body)]),
    
    %% Add context extraction at the very beginning of the clause body
    ContextExtraction = {match, Line, {var, Line, 'Context'}, 
                        {record_field, Line, {var, Line, 'State'}, state, {atom, Line, context}}},
    
    NewBody = [ContextExtraction | lists:map(fun(Expr) -> rewrite_expression_record(Expr, ModuleName) end, Body)],
    io:format("PARSE_TRANSFORM: Added context extraction at header, body now has ~p expressions~n", [length(NewBody)]),
    {clause, Line, Patterns, Guards, NewBody}.

%%── Transform gen_server:call to use simple_wrapper:call for record approach
rewrite_expression_record({call, CLine,
                   {remote, RLine, {atom,_,gen_server}, {atom,_,call}},
                   [Target, Msg]}, ModuleName) ->
    io:format("PARSE_TRANSFORM: *** FOUND gen_server:call! Transforming in module ~p ***~n", [ModuleName]),
    % Use the Context variable that was extracted at the clause header
    ContextExpr = {var, CLine, 'Context'},
    ModuleExpr = {atom, CLine, ModuleName},
    {call, CLine,
       {remote, RLine, {atom,RLine,simple_wrapper}, {atom,RLine,call}},
       [Target, Msg, ContextExpr, ModuleExpr]};

rewrite_expression_record({call, CLine, Fun, Args}, ModuleName) ->
    NewArgs = lists:map(fun(Arg) -> rewrite_expression_record(Arg, ModuleName) end, Args),
    {call, CLine, Fun, NewArgs};

rewrite_expression_record({tuple, Line, Elements}, ModuleName) ->
    NewElements = lists:map(fun(Elem) -> rewrite_expression_record(Elem, ModuleName) end, Elements),
    {tuple, Line, NewElements};

rewrite_expression_record({cons, Line, Head, Tail}, ModuleName) ->
    NewHead = rewrite_expression_record(Head, ModuleName),
    NewTail = rewrite_expression_record(Tail, ModuleName),
    {cons, Line, NewHead, NewTail};

rewrite_expression_record({'case', Line, Expr, Clauses}, ModuleName) ->
    NewExpr = rewrite_expression_record(Expr, ModuleName),
    NewClauses = lists:map(fun(Clause) -> rewrite_case_clause_record(Clause, ModuleName) end, Clauses),
    {'case', Line, NewExpr, NewClauses};

rewrite_expression_record({match, Line, Left, Right}, ModuleName) ->
    NewRight = rewrite_expression_record(Right, ModuleName),
    {match, Line, Left, NewRight};

rewrite_expression_record({'if', Line, Clauses}, ModuleName) ->
    NewClauses = lists:map(fun(Clause) -> rewrite_case_clause_record(Clause, ModuleName) end, Clauses),
    {'if', Line, NewClauses};

rewrite_expression_record({'try', LineInfo, Body, [], CatchClauses, AfterClause}, ModuleName) ->
    io:format("PARSE_TRANSFORM: Processing try block with ~p body expressions in ~p~n", [length(Body), ModuleName]),
    NewBody = lists:map(fun(Expr) -> rewrite_expression_record(Expr, ModuleName) end, Body),
    NewCatchClauses = lists:map(fun(Clause) -> rewrite_case_clause_record(Clause, ModuleName) end, CatchClauses),
    NewAfterClause = case AfterClause of
        [] -> [];
        _ -> lists:map(fun(Expr) -> rewrite_expression_record(Expr, ModuleName) end, AfterClause)
    end,
    {'try', LineInfo, NewBody, [], NewCatchClauses, NewAfterClause};

rewrite_expression_record({'receive', Line, Clauses}, ModuleName) ->
    NewClauses = lists:map(fun(Clause) -> rewrite_case_clause_record(Clause, ModuleName) end, Clauses),
    {'receive', Line, NewClauses};

rewrite_expression_record({'receive', Line, Clauses, Timeout, TimeoutBody}, ModuleName) ->
    NewClauses = lists:map(fun(Clause) -> rewrite_case_clause_record(Clause, ModuleName) end, Clauses),
    NewTimeout = rewrite_expression_record(Timeout, ModuleName),
    NewTimeoutBody = lists:map(fun(Expr) -> rewrite_expression_record(Expr, ModuleName) end, TimeoutBody),
    {'receive', Line, NewClauses, NewTimeout, NewTimeoutBody};

rewrite_expression_record({'fun', Line, {clauses, Clauses}}, ModuleName) ->
    NewClauses = lists:map(fun(Clause) -> rewrite_case_clause_record(Clause, ModuleName) end, Clauses),
    {'fun', Line, {clauses, NewClauses}};

rewrite_expression_record({lc, Line, Template, Qualifiers}, ModuleName) ->
    NewTemplate = rewrite_expression_record(Template, ModuleName),
    NewQualifiers = lists:map(fun(Qual) -> rewrite_qualifier_record(Qual, ModuleName) end, Qualifiers),
    {lc, Line, NewTemplate, NewQualifiers};

rewrite_expression_record({bc, Line, Template, Qualifiers}, ModuleName) ->
    NewTemplate = rewrite_expression_record(Template, ModuleName),
    NewQualifiers = lists:map(fun(Qual) -> rewrite_qualifier_record(Qual, ModuleName) end, Qualifiers),
    {bc, Line, NewTemplate, NewQualifiers};

rewrite_expression_record(Other, _ModuleName) ->
    Other.

rewrite_case_clause_record({clause, Line, Patterns, Guards, Body}, ModuleName) ->
    NewBody = lists:map(fun(Expr) -> rewrite_expression_record(Expr, ModuleName) end, Body),
    {clause, Line, Patterns, Guards, NewBody}.

rewrite_qualifier_record({generate, Line, Pattern, Expr}, ModuleName) ->
    NewExpr = rewrite_expression_record(Expr, ModuleName),
    {generate, Line, Pattern, NewExpr};
rewrite_qualifier_record({b_generate, Line, Pattern, Expr}, ModuleName) ->
    NewExpr = rewrite_expression_record(Expr, ModuleName),
    {b_generate, Line, Pattern, NewExpr};
rewrite_qualifier_record(Expr, ModuleName) when is_tuple(Expr) ->
    rewrite_expression_record(Expr, ModuleName);
rewrite_qualifier_record(Other, _ModuleName) ->
    Other.

%%────────────────────────────────────────────────────────────────────────────
%%── MAP-BASED APPROACH
%%────────────────────────────────────────────────────────────────────────────

%%── Inject with_context handler for map-based approach (handle_call only)
inject_with_context_handler_map(Forms) ->
    ModuleName = extract_module_name(Forms),
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            io:format("PARSE_TRANSFORM: Injecting with_context handler into handle_call/3 (map)~n"),
            
            %% Only the with_context handler - NO context call handler
            WithContextClause = {clause, Line,
               [ {tuple,Line,[{atom,Line,with_context},{var,Line,'Context'},{var,Line,'Msg'}]},
                 {var,Line,'From'},
                 {var,Line,'State'}
               ],
               [],
               [ {match,Line,{var,Line,'ReceiveTime'},
                   {call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,system_time}},
                       [{atom,Line,millisecond}]}},
                 {call,Line,{remote,Line,{atom,Line,io},{atom,Line,format}},
                     [{string,Line,"[~p ms] [~p] RECEIVED with_context: ctx=~p, msg=~p~n"},
                      {cons,Line,{var,Line,'ReceiveTime'},
                          {cons,Line,{atom,Line,ModuleName},
                              {cons,Line,{var,Line,'Context'},
                                  {cons,Line,{var,Line,'Msg'},{nil,Line}}}}}]},
                 {match,Line,{var,Line,'StateWithContext'},
                     {call,Line,{remote,Line,{atom,Line,maps},{atom,Line,put}},
                         [{atom,Line,context},{var,Line,'Context'},{var,Line,'State'}]}},
                 {call,Line,{remote,Line,{atom,Line,io},{atom,Line,format}},
                     [{string,Line,"[~p ms] [~p] Context ~p injected, delegating message ~p~n"},
                      {cons,Line,{var,Line,'ReceiveTime'},
                          {cons,Line,{atom,Line,ModuleName},
                              {cons,Line,{var,Line,'Context'},
                                  {cons,Line,{var,Line,'Msg'},{nil,Line}}}}}]},
                 {call,Line,{remote,Line,{atom,Line,ModuleName},{atom,Line,handle_call}},
                     [{var,Line,'Msg'},{var,Line,'From'},{var,Line,'StateWithContext'}]}
               ]},

            {function, Line, handle_call, 3, [WithContextClause | Clauses]};
        (Other) ->
            Other
    end, Forms).

%%── Inject context handler for map-based approach (handle_cast only)
inject_context_cast_handler_map(Forms) ->
    lists:map(fun
        ({function, Line, handle_cast, 2, Clauses}) ->
            io:format("PARSE_TRANSFORM: Injecting context handler into handle_cast/2 (map)~n"),
            
            %% Context cast handler
            ContextCastClause = {clause, Line,
               [ {tuple,Line,[{atom,Line,context},{var,Line,'Context'}]},
                 {var,Line,'State'}
               ],
               [],
               [ {tuple,Line,
                   [ {atom,Line,noreply},
                     {call,Line,{remote,Line,{atom,Line,maps},{atom,Line,put}},
                         [{atom,Line,context},{var,Line,'Context'},{var,Line,'State'}]}
                   ]}
               ]},

            {function, Line, handle_cast, 2, [ContextCastClause | Clauses]};
        (Other) ->
            Other
    end, Forms).

%%── Rewrite calls for map-based approach WITH context extraction at header
rewrite_process_calls_map(Forms, ModuleName) ->
    io:format("PARSE_TRANSFORM: Starting rewrite_process_calls_map for ~p~n", [ModuleName]),
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            %% Separate injected clauses from original clauses
            {InjectedClauses, OriginalClauses} = separate_clauses(Clauses),
            
            %% Only rewrite original clauses (not injected ones)
            NewOriginalClauses = lists:map(fun(Clause) -> rewrite_clause_map(Clause, ModuleName) end, OriginalClauses),
            
            %% Combine: injected clauses first (unchanged), then rewritten original clauses
            NewClauses = InjectedClauses ++ NewOriginalClauses,
            {function, Line, handle_call, 3, NewClauses};
        (Other) ->
            Other
    end, Forms).

rewrite_clause_map({clause, Line, Patterns, Guards, Body}, ModuleName) ->
    io:format("PARSE_TRANSFORM: Rewriting original user map clause with ~p expressions in body~n", [length(Body)]),
    
    %% Add context extraction at the very beginning of the clause body
    ContextExtraction = {match, Line, {var, Line, 'Context'}, 
                        {call,Line,{remote,Line,{atom,Line,maps},{atom,Line,get}},
                         [{atom,Line,context},{var,Line,'State'},{atom,Line,undefined}]}},
    
    NewBody = [ContextExtraction | lists:map(fun(Expr) -> rewrite_expression_map(Expr, ModuleName) end, Body)],
    io:format("PARSE_TRANSFORM: Added context extraction at header, body now has ~p expressions~n", [length(NewBody)]),
    {clause, Line, Patterns, Guards, NewBody}.

%%── Transform gen_server:call to use simple_wrapper:call for map approach
rewrite_expression_map({call, CLine,
                   {remote, RLine, {atom,_,gen_server}, {atom,_,call}},
                   [Target, Msg]}, ModuleName) ->
    io:format("PARSE_TRANSFORM: *** FOUND gen_server:call! Transforming in module ~p (map approach) ***~n", [ModuleName]),
    % Use the Context variable that was extracted at the clause header
    ContextExpr = {var, CLine, 'Context'},
    ModuleExpr = {atom, CLine, ModuleName},
    {call, CLine,
       {remote, RLine, {atom,RLine,simple_wrapper}, {atom,RLine,call}},
       [Target, Msg, ContextExpr, ModuleExpr]};

rewrite_expression_map({call, CLine, Fun, Args}, ModuleName) ->
    NewArgs = lists:map(fun(Arg) -> rewrite_expression_map(Arg, ModuleName) end, Args),
    {call, CLine, Fun, NewArgs};

rewrite_expression_map({tuple, Line, Elements}, ModuleName) ->
    NewElements = lists:map(fun(Elem) -> rewrite_expression_map(Elem, ModuleName) end, Elements),
    {tuple, Line, NewElements};

rewrite_expression_map({cons, Line, Head, Tail}, ModuleName) ->
    NewHead = rewrite_expression_map(Head, ModuleName),
    NewTail = rewrite_expression_map(Tail, ModuleName),
    {cons, Line, NewHead, NewTail};

rewrite_expression_map({'case', Line, Expr, Clauses}, ModuleName) ->
    NewExpr = rewrite_expression_map(Expr, ModuleName),
    NewClauses = lists:map(fun(Clause) -> rewrite_case_clause_map(Clause, ModuleName) end, Clauses),
    {'case', Line, NewExpr, NewClauses};

rewrite_expression_map({match, Line, Left, Right}, ModuleName) ->
    NewRight = rewrite_expression_map(Right, ModuleName),
    {match, Line, Left, NewRight};

rewrite_expression_map({'if', Line, Clauses}, ModuleName) ->
    NewClauses = lists:map(fun(Clause) -> rewrite_case_clause_map(Clause, ModuleName) end, Clauses),
    {'if', Line, NewClauses};

rewrite_expression_map({'try', LineInfo, Body, [], CatchClauses, AfterClause}, ModuleName) ->
    io:format("PARSE_TRANSFORM: Processing try block with ~p body expressions in ~p (map)~n", [length(Body), ModuleName]),
    NewBody = lists:map(fun(Expr) -> rewrite_expression_map(Expr, ModuleName) end, Body),
    NewCatchClauses = lists:map(fun(Clause) -> rewrite_case_clause_map(Clause, ModuleName) end, CatchClauses),
    NewAfterClause = case AfterClause of
        [] -> [];
        _ -> lists:map(fun(Expr) -> rewrite_expression_map(Expr, ModuleName) end, AfterClause)
    end,
    {'try', LineInfo, NewBody, [], NewCatchClauses, NewAfterClause};

rewrite_expression_map({'receive', Line, Clauses}, ModuleName) ->
    NewClauses = lists:map(fun(Clause) -> rewrite_case_clause_map(Clause, ModuleName) end, Clauses),
    {'receive', Line, NewClauses};

rewrite_expression_map({'receive', Line, Clauses, Timeout, TimeoutBody}, ModuleName) ->
    NewClauses = lists:map(fun(Clause) -> rewrite_case_clause_map(Clause, ModuleName) end, Clauses),
    NewTimeout = rewrite_expression_map(Timeout, ModuleName),
    NewTimeoutBody = lists:map(fun(Expr) -> rewrite_expression_map(Expr, ModuleName) end, TimeoutBody),
    {'receive', Line, NewClauses, NewTimeout, NewTimeoutBody};

rewrite_expression_map({'fun', Line, {clauses, Clauses}}, ModuleName) ->
    NewClauses = lists:map(fun(Clause) -> rewrite_case_clause_map(Clause, ModuleName) end, Clauses),
    {'fun', Line, {clauses, NewClauses}};

rewrite_expression_map({lc, Line, Template, Qualifiers}, ModuleName) ->
    NewTemplate = rewrite_expression_map(Template, ModuleName),
    NewQualifiers = lists:map(fun(Qual) -> rewrite_qualifier_map(Qual, ModuleName) end, Qualifiers),
    {lc, Line, NewTemplate, NewQualifiers};

rewrite_expression_map({bc, Line, Template, Qualifiers}, ModuleName) ->
    NewTemplate = rewrite_expression_map(Template, ModuleName),
    NewQualifiers = lists:map(fun(Qual) -> rewrite_qualifier_map(Qual, ModuleName) end, Qualifiers),
    {bc, Line, NewTemplate, NewQualifiers};

rewrite_expression_map(Other, _ModuleName) ->
    Other.

rewrite_case_clause_map({clause, Line, Patterns, Guards, Body}, ModuleName) ->
    NewBody = lists:map(fun(Expr) -> rewrite_expression_map(Expr, ModuleName) end, Body),
    {clause, Line, Patterns, Guards, NewBody}.

rewrite_qualifier_map({generate, Line, Pattern, Expr}, ModuleName) ->
    NewExpr = rewrite_expression_map(Expr, ModuleName),
    {generate, Line, Pattern, NewExpr};
rewrite_qualifier_map({b_generate, Line, Pattern, Expr}, ModuleName) ->
    NewExpr = rewrite_expression_map(Expr, ModuleName),
    {b_generate, Line, Pattern, NewExpr};
rewrite_qualifier_map(Expr, ModuleName) when is_tuple(Expr) ->
    rewrite_expression_map(Expr, ModuleName);
rewrite_qualifier_map(Other, _ModuleName) ->
    Other.