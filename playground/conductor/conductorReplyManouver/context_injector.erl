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
                    Forms2 = inject_with_context_handler_only_record(Forms1),
                    rewrite_process_calls_record(Forms2, ModuleName);
                map ->
                    io:format("PARSE_TRANSFORM: Using map-based approach for ~p~n", [ModuleName]),
                    Forms1 = inject_with_context_handler_only_map(Forms),
                    rewrite_process_calls_map(Forms1, ModuleName)
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

ensure_context_in_state_record(Forms) ->
    case find_state_record(Forms) of
        {found, _RecordForm} ->
            io:format("PARSE_TRANSFORM: Found existing state record, checking for context field~n"),
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
    RecordDef = {attribute, 1, record, {state, [
        {record_field, 1, {atom, 1, context}, {atom, 1, undefined}}
    ]}},
    lists:reverse([RecordDef | Acc]);

inject_state_record([{attribute, Line, module, _ModName} = ModAttr | Rest], Acc) ->
    RecordDef = {attribute, Line, record, {state, [
        {record_field, Line, {atom, Line, context}, {atom, Line, undefined}}
    ]}},
    lists:reverse(Acc) ++ [ModAttr, RecordDef | Rest];

inject_state_record([Form | Rest], Acc) ->
    inject_state_record(Rest, [Form | Acc]).

%%── Inject ONLY with_context handler for record-based approach (no {context, Context} handler)
inject_with_context_handler_only_record(Forms) ->
    ModuleName = extract_module_name(Forms),
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            io:format("PARSE_TRANSFORM: Injecting with_context handler into handle_call/3 (record)~n"),
            
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
                 {call,Line,{remote,Line,{atom,Line,ModuleName},{atom,Line,handle_call}},
                     [{var,Line,'Msg'},{var,Line,'From'},{var,Line,'StateWithContext'}]}
               ]},

            {function, Line, handle_call, 3, [WithContextClause | Clauses]};
        (Other) ->
            Other
    end, Forms).

rewrite_process_calls_record(Forms, ModuleName) ->
    io:format("PARSE_TRANSFORM: Starting rewrite_process_calls_record for ~p~n", [ModuleName]),
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            {InjectedClauses, OriginalClauses} = separate_clauses(Clauses),
            NewOriginalClauses = lists:map(fun(Clause) -> 
                rewrite_clause_record_with_tuples(Clause, ModuleName) 
            end, OriginalClauses),
            NewClauses = InjectedClauses ++ NewOriginalClauses,
            {function, Line, handle_call, 3, NewClauses};
        (Other) ->
            Other
    end, Forms).

%%────────────────────────────────────────────────────────────────────────────
%%── MAP-BASED APPROACH
%%────────────────────────────────────────────────────────────────────────────

inject_with_context_handler_only_map(Forms) ->
    ModuleName = extract_module_name(Forms),
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            io:format("PARSE_TRANSFORM: Injecting with_context handler into handle_call/3 (map)~n"),
            
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

rewrite_process_calls_map(Forms, ModuleName) ->
    io:format("PARSE_TRANSFORM: Starting rewrite_process_calls_map for ~p~n", [ModuleName]),
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            {InjectedClauses, OriginalClauses} = separate_clauses(Clauses),
            NewOriginalClauses = lists:map(fun(Clause) -> 
                rewrite_clause_map_with_tuples(Clause, ModuleName) 
            end, OriginalClauses),
            NewClauses = InjectedClauses ++ NewOriginalClauses,
            {function, Line, handle_call, 3, NewClauses};
        (Other) ->
            Other
    end, Forms).

%%────────────────────────────────────────────────────────────────────────────
%%── SHARED UTILITIES
%%────────────────────────────────────────────────────────────────────────────

separate_clauses(Clauses) ->
    lists:partition(fun(Clause) -> is_injected_clause(Clause) end, Clauses).

is_injected_clause({clause, _, [{tuple, _, [{atom, _, with_context}, _, _]}, _, _], _, _}) ->
    true;
is_injected_clause(_) ->
    false.

%%── NEW: Clause rewriting with tuple destructuring for record approach
rewrite_clause_record_with_tuples({clause, Line, Patterns, Guards, Body}, ModuleName) ->
    io:format("PARSE_TRANSFORM: Rewriting record clause with tuple destructuring for ~p~n", [ModuleName]),
    
    %% Add context extraction at the beginning
    ContextExtraction = {match, Line, {var, Line, 'Context'}, 
                        {record_field, Line, {var, Line, 'State'}, state, {atom, Line, context}}},
    
    %% Process body to create tuple destructuring and state updates
    {TransformedBody, _FinalState} = process_body_with_tuple_destructuring(Body, ModuleName, record, 1, 'State'),
    
    {clause, Line, Patterns, Guards, [ContextExtraction | TransformedBody]}.

%%── NEW: Clause rewriting with tuple destructuring for map approach
rewrite_clause_map_with_tuples({clause, Line, Patterns, Guards, Body}, ModuleName) ->
    io:format("PARSE_TRANSFORM: Rewriting map clause with tuple destructuring for ~p~n", [ModuleName]),
    
    %% Add context extraction at the beginning
    ContextExtraction = {match, Line, {var, Line, 'Context'}, 
                        {call,Line,{remote,Line,{atom,Line,maps},{atom,Line,get}},
                         [{atom,Line,context},{var,Line,'State'},{atom,Line,undefined}]}},
    
    %% Process body to create tuple destructuring and state updates
    {TransformedBody, _FinalState} = process_body_with_tuple_destructuring(Body, ModuleName, map, 1, 'State'),
    
    {clause, Line, Patterns, Guards, [ContextExtraction | TransformedBody]}.

%%── Process clause body creating tuple destructuring for gen_server:call patterns
process_body_with_tuple_destructuring(Body, ModuleName, StateType, Counter, CurrentStateVar) ->
    process_body_with_tuple_destructuring(Body, ModuleName, StateType, Counter, CurrentStateVar, []).

process_body_with_tuple_destructuring([], _ModuleName, _StateType, _Counter, FinalStateVar, Acc) ->
    {lists:reverse(Acc), FinalStateVar};

process_body_with_tuple_destructuring([Expr | Rest], ModuleName, StateType, Counter, CurrentStateVar, Acc) ->
    case Expr of
        %% Pattern: VarName = gen_server:call(Target, Msg)
        {match, Line, {var, _, VarName}, {call, _, {remote, _, {atom,_,gen_server}, {atom,_,call}}, [Target, Msg]}} ->
            io:format("PARSE_TRANSFORM: Found gen_server:call assignment to ~p in ~p~n", [VarName, ModuleName]),
            
            %% Generate unique variables
            ContextVar = list_to_atom("Context" ++ integer_to_list(Counter)),
            NewStateVar = list_to_atom("State" ++ integer_to_list(Counter)),
            
            %% Create tuple match: {VarName, Context1} = simple_wrapper:call(...)
            TupleMatch = {match, Line,
                         {tuple, Line, [{var, Line, VarName}, {var, Line, ContextVar}]},
                         {call, Line, {remote, Line, {atom, Line, simple_wrapper}, {atom, Line, call}},
                          [Target, Msg, {var, Line, 'Context'}, {atom, Line, ModuleName}]}},
            
            %% Create state update: State1 = State#{context => Context1}
            StateUpdate = case StateType of
                record ->
                    {match, Line, {var, Line, NewStateVar},
                     {record, Line, {var, Line, CurrentStateVar}, state,
                      [{record_field, Line, {atom, Line, context}, {var, Line, ContextVar}}]}};
                map ->
                    {match, Line, {var, Line, NewStateVar},
                     {call, Line, {remote, Line, {atom, Line, maps}, {atom, Line, put}},
                      [{atom, Line, context}, {var, Line, ContextVar}, {var, Line, CurrentStateVar}]}}
            end,
            
            %% Continue with updated state variable
            process_body_with_tuple_destructuring(Rest, ModuleName, StateType, Counter + 1, NewStateVar,
                                                [StateUpdate, TupleMatch | Acc]);
        
        %% Pattern: {reply, Reply, State}
        {tuple, Line, [{atom, _, reply}, ReplyExpr, {var, _, 'State'}]} ->
            %% Update to use current state variable
            UpdatedExpr = {tuple, Line, [{atom, Line, reply}, ReplyExpr, {var, Line, CurrentStateVar}]},
            process_body_with_tuple_destructuring(Rest, ModuleName, StateType, Counter, CurrentStateVar, [UpdatedExpr | Acc]);
        
        %% Pattern: {noreply, State}
        {tuple, Line, [{atom, _, noreply}, {var, _, 'State'}]} ->
            %% Update to use current state variable
            UpdatedExpr = {tuple, Line, [{atom, Line, noreply}, {var, Line, CurrentStateVar}]},
            process_body_with_tuple_destructuring(Rest, ModuleName, StateType, Counter, CurrentStateVar, [UpdatedExpr | Acc]);
        
        %% All other expressions - use the working deep transformation logic but without gen_server:call replacement
        _ ->
            TransformedExpr = deep_transform_expression_without_genserver_call(Expr, ModuleName, StateType),
            process_body_with_tuple_destructuring(Rest, ModuleName, StateType, Counter, CurrentStateVar, [TransformedExpr | Acc])
    end.

%%── Deep transformation logic from your working code, but WITHOUT transforming gen_server:call
%%   (since we handle that at the clause level now)
deep_transform_expression_without_genserver_call({call, CLine, Fun, Args}, ModuleName, StateType) ->
    %% Transform arguments recursively but don't transform gen_server:call itself
    case Fun of
        {remote, _, {atom,_,gen_server}, {atom,_,call}} ->
            %% This is gen_server:call - should be handled at clause level, so this is an error case
            io:format("PARSE_TRANSFORM: WARNING: gen_server:call found in deep transform - should be handled at clause level~n"),
            %% For safety, transform it the old way
            ContextExpr = {var, CLine, 'Context'},
            ModuleExpr = {atom, CLine, ModuleName},
            {call, CLine, {remote, element(2, Fun), {atom, element(2, Fun), simple_wrapper}, {atom, element(2, Fun), call}},
             Args ++ [ContextExpr, ModuleExpr]};
        _ ->
            NewFun = deep_transform_expression_without_genserver_call(Fun, ModuleName, StateType),
            NewArgs = lists:map(fun(Arg) -> deep_transform_expression_without_genserver_call(Arg, ModuleName, StateType) end, Args),
            {call, CLine, NewFun, NewArgs}
    end;

deep_transform_expression_without_genserver_call({tuple, Line, Elements}, ModuleName, StateType) ->
    NewElements = lists:map(fun(Elem) -> deep_transform_expression_without_genserver_call(Elem, ModuleName, StateType) end, Elements),
    {tuple, Line, NewElements};

deep_transform_expression_without_genserver_call({cons, Line, Head, Tail}, ModuleName, StateType) ->
    NewHead = deep_transform_expression_without_genserver_call(Head, ModuleName, StateType),
    NewTail = deep_transform_expression_without_genserver_call(Tail, ModuleName, StateType),
    {cons, Line, NewHead, NewTail};

deep_transform_expression_without_genserver_call({'case', Line, Expr, Clauses}, ModuleName, StateType) ->
    NewExpr = deep_transform_expression_without_genserver_call(Expr, ModuleName, StateType),
    NewClauses = lists:map(fun({clause, CLine, CPatterns, CGuards, CBody}) ->
        NewCBody = lists:map(fun(CExpr) -> deep_transform_expression_without_genserver_call(CExpr, ModuleName, StateType) end, CBody),
        {clause, CLine, CPatterns, CGuards, NewCBody}
    end, Clauses),
    {'case', Line, NewExpr, NewClauses};

deep_transform_expression_without_genserver_call({match, Line, Left, Right}, ModuleName, StateType) ->
    NewRight = deep_transform_expression_without_genserver_call(Right, ModuleName, StateType),
    {match, Line, Left, NewRight};

deep_transform_expression_without_genserver_call({'if', Line, Clauses}, ModuleName, StateType) ->
    NewClauses = lists:map(fun({clause, CLine, CPatterns, CGuards, CBody}) ->
        NewCBody = lists:map(fun(CExpr) -> deep_transform_expression_without_genserver_call(CExpr, ModuleName, StateType) end, CBody),
        {clause, CLine, CPatterns, CGuards, NewCBody}
    end, Clauses),
    {'if', Line, NewClauses};

deep_transform_expression_without_genserver_call({'try', LineInfo, Body, [], CatchClauses, AfterClause}, ModuleName, StateType) ->
    NewBody = lists:map(fun(Expr) -> deep_transform_expression_without_genserver_call(Expr, ModuleName, StateType) end, Body),
    NewCatchClauses = lists:map(fun({clause, CLine, CPatterns, CGuards, CBody}) ->
        NewCBody = lists:map(fun(CExpr) -> deep_transform_expression_without_genserver_call(CExpr, ModuleName, StateType) end, CBody),
        {clause, CLine, CPatterns, CGuards, NewCBody}
    end, CatchClauses),
    NewAfterClause = case AfterClause of
        [] -> [];
        _ -> lists:map(fun(Expr) -> deep_transform_expression_without_genserver_call(Expr, ModuleName, StateType) end, AfterClause)
    end,
    {'try', LineInfo, NewBody, [], NewCatchClauses, NewAfterClause};

deep_transform_expression_without_genserver_call({'receive', Line, Clauses}, ModuleName, StateType) ->
    NewClauses = lists:map(fun({clause, CLine, CPatterns, CGuards, CBody}) ->
        NewCBody = lists:map(fun(CExpr) -> deep_transform_expression_without_genserver_call(CExpr, ModuleName, StateType) end, CBody),
        {clause, CLine, CPatterns, CGuards, NewCBody}
    end, Clauses),
    {'receive', Line, NewClauses};

deep_transform_expression_without_genserver_call({'receive', Line, Clauses, Timeout, TimeoutBody}, ModuleName, StateType) ->
    NewClauses = lists:map(fun({clause, CLine, CPatterns, CGuards, CBody}) ->
        NewCBody = lists:map(fun(CExpr) -> deep_transform_expression_without_genserver_call(CExpr, ModuleName, StateType) end, CBody),
        {clause, CLine, CPatterns, CGuards, NewCBody}
    end, Clauses),
    NewTimeout = deep_transform_expression_without_genserver_call(Timeout, ModuleName, StateType),
    NewTimeoutBody = lists:map(fun(Expr) -> deep_transform_expression_without_genserver_call(Expr, ModuleName, StateType) end, TimeoutBody),
    {'receive', Line, NewClauses, NewTimeout, NewTimeoutBody};

deep_transform_expression_without_genserver_call({'fun', Line, {clauses, Clauses}}, ModuleName, StateType) ->
    NewClauses = lists:map(fun({clause, CLine, CPatterns, CGuards, CBody}) ->
        NewCBody = lists:map(fun(CExpr) -> deep_transform_expression_without_genserver_call(CExpr, ModuleName, StateType) end, CBody),
        {clause, CLine, CPatterns, CGuards, NewCBody}
    end, Clauses),
    {'fun', Line, {clauses, NewClauses}};

deep_transform_expression_without_genserver_call({lc, Line, Template, Qualifiers}, ModuleName, StateType) ->
    NewTemplate = deep_transform_expression_without_genserver_call(Template, ModuleName, StateType),
    NewQualifiers = lists:map(fun(Qual) ->
        case Qual of
            {generate, GLine, Pattern, Expr} ->
                {generate, GLine, Pattern, deep_transform_expression_without_genserver_call(Expr, ModuleName, StateType)};
            {b_generate, GLine, Pattern, Expr} ->
                {b_generate, GLine, Pattern, deep_transform_expression_without_genserver_call(Expr, ModuleName, StateType)};
            _ when is_tuple(Qual) ->
                deep_transform_expression_without_genserver_call(Qual, ModuleName, StateType);
            _ ->
                Qual
        end
    end, Qualifiers),
    {lc, Line, NewTemplate, NewQualifiers};

deep_transform_expression_without_genserver_call({bc, Line, Template, Qualifiers}, ModuleName, StateType) ->
    NewTemplate = deep_transform_expression_without_genserver_call(Template, ModuleName, StateType),
    NewQualifiers = lists:map(fun(Qual) ->
        case Qual of
            {generate, GLine, Pattern, Expr} ->
                {generate, GLine, Pattern, deep_transform_expression_without_genserver_call(Expr, ModuleName, StateType)};
            {b_generate, GLine, Pattern, Expr} ->
                {b_generate, GLine, Pattern, deep_transform_expression_without_genserver_call(Expr, ModuleName, StateType)};
            _ when is_tuple(Qual) ->
                deep_transform_expression_without_genserver_call(Qual, ModuleName, StateType);
            _ ->
                Qual
        end
    end, Qualifiers),
    {bc, Line, NewTemplate, NewQualifiers};

deep_transform_expression_without_genserver_call(Other, _ModuleName, _StateType) ->
    Other.