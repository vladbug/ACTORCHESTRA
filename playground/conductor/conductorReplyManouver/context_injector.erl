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
                    Forms2 = inject_context_handlers_record(Forms1, ModuleName),
                    rewrite_process_calls_record(Forms2, ModuleName);
                map ->
                    io:format("PARSE_TRANSFORM: Using map-based approach for ~p~n", [ModuleName]),
                    Forms1 = inject_context_handlers_map(Forms, ModuleName),
                    rewrite_process_calls_map(Forms1, ModuleName)
            end;
        false ->
            io:format("PARSE_TRANSFORM: Skipping non-gen_server module~n"),
            Forms
    end.

%%—————————————————————————————————————————————————————————————————————————————————————————————————————
%%— UTILITY FUNCTIONS
%%—————————————————————————————————————————————————————————————————————————————————————————————————————

extract_module_name([{attribute, _, module, ModuleName} | _]) ->
    ModuleName;
extract_module_name([_ | Rest]) ->
    extract_module_name(Rest);
extract_module_name([]) ->
    undefined.

is_gen_server(Forms) ->
    lists:any(fun
        ({attribute,_,behaviour,gen_server}) -> true;
        ({attribute,_,behavior,gen_server}) -> true;
        (_) -> false
    end, Forms).

detect_state_approach(Forms) ->
    case find_state_record(Forms) of
        {found, _} -> record;
        not_found -> map
    end.

is_client_module(client) -> true;
is_client_module(_) -> false.

find_state_record([{attribute, _Line, record, {state, _Fields}} = Record | _]) ->
    {found, Record};
find_state_record([_ | Rest]) ->
    find_state_record(Rest);
find_state_record([]) ->
    not_found.

separate_clauses(Clauses) ->
    lists:partition(fun(Clause) -> is_injected_clause(Clause) end, Clauses).

is_injected_clause({clause, _, [{tuple, _, [{atom, _, with_context}, _, _]}, _, _], _, _}) ->
    true;
is_injected_clause({clause, _, [{atom, _, get_current_context}, _, _], _, _}) ->
    true;
is_injected_clause({clause, _, [{tuple, _, [{atom, _, context}, _]}, _, _], _, _}) ->
    true;
is_injected_clause(_) ->
    false.

%% Check if body contains any gen_server:call expressions
has_gen_server_call([]) ->
    false;
has_gen_server_call([{match, _, _, {call, _, {remote, _, {atom,_,gen_server}, {atom,_,call}}, _}} | _]) ->
    true;
has_gen_server_call([_ | Rest]) ->
    has_gen_server_call(Rest).

%%—————————————————————————————————————————————————————————————————————————————————————————————————————
%%— RECORD-BASED APPROACH
%%—————————————————————————————————————————————————————————————————————————————————————————————————————

ensure_context_in_state_record(Forms) ->
    case find_state_record(Forms) of
        {found, _} ->
            modify_forms_for_context(Forms);
        not_found ->
            inject_state_record(Forms)
    end.

modify_forms_for_context(Forms) ->
    lists:map(fun
        ({attribute, Line, record, {state, Fields}} = _Form) ->
            case has_context_field(Fields) of
                true ->
                    {attribute, Line, record, {state, Fields}};
                false ->
                    ContextField = {record_field, Line, {atom, Line, context}, {atom, Line, undefined}},
                    {attribute, Line, record, {state, Fields ++ [ContextField]}}
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

inject_context_handlers_record(Forms, ModuleName) ->
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            io:format("PARSE_TRANSFORM: Injecting context handlers into handle_call/3 (record)~n"),
            NewClauses = case is_client_module(ModuleName) of
                true ->
                    Clauses;
                false ->
                    %% For non-client modules, add with_context handler
                    WithContextClause = {clause, Line,
                       [{tuple,Line,[{atom,Line,with_context},{var,Line,'Context'},{var,Line,'Msg'}]},
                        {var,Line,'From'}, {var,Line,'State'}],
                       [],
                       [{match,Line,{var,Line,'StateWithContext'},
                            {record,Line,{var,Line,'State'},state,
                                [{record_field,Line,{atom,Line,context},{var,Line,'Context'}}]}},
                        {call,Line,{remote,Line,{atom,Line,ModuleName},{atom,Line,handle_call}},
                            [{var,Line,'Msg'},{var,Line,'From'},{var,Line,'StateWithContext'}]}]},
                    [WithContextClause | Clauses]
            end,
            {function, Line, handle_call, 3, NewClauses};
        (Other) ->
            Other
    end, Forms).

rewrite_process_calls_record(Forms, ModuleName) ->
    io:format("PARSE_TRANSFORM: Starting rewrite_process_calls_record for ~p~n", [ModuleName]),
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            {InjectedClauses, OriginalClauses} = separate_clauses(Clauses),
            NewOriginalClauses = lists:map(fun(Clause) -> 
                rewrite_clause_record(Clause, ModuleName) 
            end, OriginalClauses),
            {function, Line, handle_call, 3, InjectedClauses ++ NewOriginalClauses};
        (Other) ->
            Other
    end, Forms).

rewrite_clause_record({clause, Line, Patterns, Guards, Body}, ModuleName) ->
    io:format("PARSE_TRANSFORM: Rewriting clause for ~p~n", [ModuleName]),
    
    %% Always add context extraction as first statement
    ContextExtraction = {match, Line, {var, Line, 'Context'}, 
                        {record_field, Line, {var, Line, 'State'}, state, {atom, Line, context}}},
    
    %% Transform body based on module type
    NewBody = case is_client_module(ModuleName) of
        true ->
            io:format("PARSE_TRANSFORM: CLIENT - transforming with tuple handling~n"),
            transform_client_body_record(Body);
        false ->
            transform_regular_body_record(Body, ModuleName)
    end,
    
    {clause, Line, Patterns, Guards, [ContextExtraction | NewBody]}.

transform_client_body_record(Body) ->
    %% Check if there are any gen_server:call expressions in the body
    case has_gen_server_call(Body) of
        true ->
            %% Has wrapper calls - transform everything
            transform_client_body_record_with_context(Body, []);
        false ->
            %% No wrapper calls - return body unchanged
            Body
    end.

transform_client_body_record_with_context([], Acc) ->
    lists:reverse(Acc);
transform_client_body_record_with_context([Expr | Rest], Acc) ->
    case Expr of
        %% Pattern: Variable = gen_server:call(...)
        {match, _MLine, {var, VLine, VarName}, 
         {call, CLine, {remote, _RLine, {atom,_,gen_server}, {atom,_,call}}, [Target, Msg]}} ->
            TupleMatch = {match, VLine,
                         {tuple, VLine, [{var, VLine, VarName}, {var, VLine, 'NewContext'}]},
                         {call, CLine, {remote, CLine, {atom,CLine,simple_wrapper}, {atom,CLine,call}},
                          [Target, Msg, {var, CLine, 'Context'}, {atom, CLine, client}]}},
            transform_client_body_record_with_context(Rest, [TupleMatch | Acc]);
        
        %% Pattern: {reply, _, _} - Transform to include context update  
        {tuple, Line, [{atom, _, reply}, ReplyVar, StateVar]} ->
            NewStateTuple = {tuple, Line, [
                {atom, Line, reply},
                ReplyVar,
                {record, Line, StateVar, state,
                 [{record_field, Line, {atom, Line, context}, {var, Line, 'NewContext'}}]}
            ]},
            transform_client_body_record_with_context(Rest, [NewStateTuple | Acc]);
        
        %% Other expressions - keep unchanged
        _ ->
            transform_client_body_record_with_context(Rest, [Expr | Acc])
    end.

transform_regular_body_record(Body, ModuleName) ->
    lists:map(fun(Expr) -> transform_expr_record(Expr, ModuleName) end, Body).

transform_expr_record(Expr, ModuleName) ->
    case Expr of
        %% Direct gen_server:call - transform to simple_wrapper:call
        {call, CLine, {remote, RLine, {atom,_,gen_server}, {atom,_,call}}, [Target, Msg]} ->
            {call, CLine, {remote, RLine, {atom,RLine,simple_wrapper}, {atom,RLine,call}},
             [Target, Msg, {var, CLine, 'Context'}, {atom, CLine, ModuleName}]};
        
        %% Assignment: Var = gen_server:call(...) - ONLY for CLIENT modules use tuple destructuring
        {match, MLine, Var, {call, CLine, {remote, RLine, {atom,_,gen_server}, {atom,_,call}}, [Target, Msg]}} ->
            WrapperCall = {call, CLine, {remote, RLine, {atom,RLine,simple_wrapper}, {atom,RLine,call}},
                          [Target, Msg, {var, CLine, 'Context'}, {atom, CLine, ModuleName}]},
            case is_client_module(ModuleName) of
                true ->
                    %% For client: {Result, NewContext} = simple_wrapper:call(...)
                    TuplePattern = {tuple, MLine, [Var, {var, MLine, '_NewContext'}]},
                    {match, MLine, TuplePattern, WrapperCall};
                false ->
                    %% For non-client: Result = simple_wrapper:call(...) (keep original pattern)
                    {match, MLine, Var, WrapperCall}
            end;
        
        %% Handle all function calls recursively
        {call, Line, Fun, Args} ->
            NewFun = transform_expr_record(Fun, ModuleName),
            NewArgs = lists:map(fun(Arg) -> transform_expr_record(Arg, ModuleName) end, Args),
            {call, Line, NewFun, NewArgs};
        
        %% Handle spawn calls specially
        {call, Line, {atom, _, spawn}, [Fun]} ->
            {call, Line, {atom, Line, spawn}, [transform_expr_record(Fun, ModuleName)]};
        
        %% Handle spawn/3 calls
        {call, Line, {atom, _, spawn}, [Module, Function, Args]} ->
            NewArgs = lists:map(fun(Arg) -> transform_expr_record(Arg, ModuleName) end, Args),
            {call, Line, {atom, Line, spawn}, [Module, Function, NewArgs]};
        
        %% Handle other compound expressions recursively
        {match, Line, Pattern, SubExpr} ->
            {match, Line, Pattern, transform_expr_record(SubExpr, ModuleName)};
        
        %% Handle try blocks with ALL possible structures
        {'try', Line, TryBody, CatchExpr, CatchClauses, AfterBody} ->
            NewTryBody = lists:map(fun(E) -> transform_expr_record(E, ModuleName) end, TryBody),
            NewCatchClauses = lists:map(fun({clause, CLine, Patterns, Guards, ClauseBody}) ->
                {clause, CLine, Patterns, Guards, lists:map(fun(E) -> transform_expr_record(E, ModuleName) end, ClauseBody)}
            end, CatchClauses),
            NewAfterBody = case AfterBody of
                [] -> [];
                _ -> lists:map(fun(E) -> transform_expr_record(E, ModuleName) end, AfterBody)
            end,
            {'try', Line, NewTryBody, CatchExpr, NewCatchClauses, NewAfterBody};
        
        %% Handle case expressions
        {'case', Line, CaseExpr, CaseClauses} ->
            NewCaseExpr = transform_expr_record(CaseExpr, ModuleName),
            NewCaseClauses = lists:map(fun({clause, CLine, Patterns, Guards, ClauseBody}) ->
                {clause, CLine, Patterns, Guards, lists:map(fun(E) -> transform_expr_record(E, ModuleName) end, ClauseBody)}
            end, CaseClauses),
            {'case', Line, NewCaseExpr, NewCaseClauses};
        
        %% Handle if expressions
        {'if', Line, IfClauses} ->
            NewIfClauses = lists:map(fun({clause, CLine, [], Guards, ClauseBody}) ->
                {clause, CLine, [], Guards, lists:map(fun(E) -> transform_expr_record(E, ModuleName) end, ClauseBody)}
            end, IfClauses),
            {'if', Line, NewIfClauses};
        
        %% Handle receive expressions
        {'receive', Line, ReceiveClauses} ->
            NewReceiveClauses = lists:map(fun({clause, CLine, Patterns, Guards, ClauseBody}) ->
                {clause, CLine, Patterns, Guards, lists:map(fun(E) -> transform_expr_record(E, ModuleName) end, ClauseBody)}
            end, ReceiveClauses),
            {'receive', Line, NewReceiveClauses};
        
        %% Handle receive with timeout
        {'receive', Line, ReceiveClauses, Timeout, TimeoutBody} ->
            NewReceiveClauses = lists:map(fun({clause, CLine, Patterns, Guards, ClauseBody}) ->
                {clause, CLine, Patterns, Guards, lists:map(fun(E) -> transform_expr_record(E, ModuleName) end, ClauseBody)}
            end, ReceiveClauses),
            NewTimeout = transform_expr_record(Timeout, ModuleName),
            NewTimeoutBody = lists:map(fun(E) -> transform_expr_record(E, ModuleName) end, TimeoutBody),
            {'receive', Line, NewReceiveClauses, NewTimeout, NewTimeoutBody};
        
        %% Handle fun expressions
        {'fun', Line, {clauses, FunClauses}} ->
            NewFunClauses = lists:map(fun({clause, CLine, Patterns, Guards, ClauseBody}) ->
                {clause, CLine, Patterns, Guards, lists:map(fun(E) -> transform_expr_record(E, ModuleName) end, ClauseBody)}
            end, FunClauses),
            {'fun', Line, {clauses, NewFunClauses}};
        
        %% Handle list comprehensions
        {lc, Line, Template, Qualifiers} ->
            NewTemplate = transform_expr_record(Template, ModuleName),
            NewQualifiers = lists:map(fun
                ({generate, QLine, Pattern, QExpr}) ->
                    {generate, QLine, Pattern, transform_expr_record(QExpr, ModuleName)};
                ({b_generate, QLine, Pattern, QExpr}) ->
                    {b_generate, QLine, Pattern, transform_expr_record(QExpr, ModuleName)};
                (QExpr) when is_tuple(QExpr) ->
                    transform_expr_record(QExpr, ModuleName);
                (Other) ->
                    Other
            end, Qualifiers),
            {lc, Line, NewTemplate, NewQualifiers};
        
        %% Handle binary comprehensions
        {bc, Line, Template, Qualifiers} ->
            NewTemplate = transform_expr_record(Template, ModuleName),
            NewQualifiers = lists:map(fun
                ({generate, QLine, Pattern, QExpr}) ->
                    {generate, QLine, Pattern, transform_expr_record(QExpr, ModuleName)};
                ({b_generate, QLine, Pattern, QExpr}) ->
                    {b_generate, QLine, Pattern, transform_expr_record(QExpr, ModuleName)};
                (QExpr) when is_tuple(QExpr) ->
                    transform_expr_record(QExpr, ModuleName);
                (Other) ->
                    Other
            end, Qualifiers),
            {bc, Line, NewTemplate, NewQualifiers};
        
        %% Handle tuples
        {tuple, Line, Elements} ->
            NewElements = lists:map(fun(Elem) -> transform_expr_record(Elem, ModuleName) end, Elements),
            {tuple, Line, NewElements};
        
        %% Handle lists
        {cons, Line, Head, Tail} ->
            NewHead = transform_expr_record(Head, ModuleName),
            NewTail = transform_expr_record(Tail, ModuleName),
            {cons, Line, NewHead, NewTail};
        
        %% Handle blocks
        {block, Line, BlockBody} ->
            NewBlockBody = lists:map(fun(E) -> transform_expr_record(E, ModuleName) end, BlockBody),
            {block, Line, NewBlockBody};
        
        %% Handle catch expressions
        {'catch', Line, CatchExpr} ->
            {'catch', Line, transform_expr_record(CatchExpr, ModuleName)};
        
        %% Default case - return as is
        _ ->
            Expr
    end.

%%—————————————————————————————————————————————————————————————————————————————————————————————————————
%%— MAP-BASED APPROACH
%%—————————————————————————————————————————————————————————————————————————————————————————————————————

inject_context_handlers_map(Forms, ModuleName) ->
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            io:format("PARSE_TRANSFORM: Injecting context handlers into handle_call/3 (map)~n"),
            NewClauses = case is_client_module(ModuleName) of
                true ->
                    Clauses;
                false ->
                    %% For non-client modules, add with_context handler
                    WithContextClause = {clause, Line,
                       [{tuple,Line,[{atom,Line,with_context},{var,Line,'Context'},{var,Line,'Msg'}]},
                        {var,Line,'From'}, {var,Line,'State'}],
                       [],
                       [{match,Line,{var,Line,'StateWithContext'},
                            {call,Line,{remote,Line,{atom,Line,maps},{atom,Line,put}},
                                [{atom,Line,context},{var,Line,'Context'},{var,Line,'State'}]}},
                        {call,Line,{remote,Line,{atom,Line,ModuleName},{atom,Line,handle_call}},
                            [{var,Line,'Msg'},{var,Line,'From'},{var,Line,'StateWithContext'}]}]},
                    [WithContextClause | Clauses]
            end,
            {function, Line, handle_call, 3, NewClauses};
        (Other) ->
            Other
    end, Forms).

rewrite_process_calls_map(Forms, ModuleName) ->
    io:format("PARSE_TRANSFORM: Starting rewrite_process_calls_map for ~p~n", [ModuleName]),
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            {InjectedClauses, OriginalClauses} = separate_clauses(Clauses),
            NewOriginalClauses = lists:map(fun(Clause) -> 
                rewrite_clause_map(Clause, ModuleName) 
            end, OriginalClauses),
            {function, Line, handle_call, 3, InjectedClauses ++ NewOriginalClauses};
        (Other) ->
            Other
    end, Forms).

rewrite_clause_map({clause, Line, Patterns, Guards, Body}, ModuleName) ->
    io:format("PARSE_TRANSFORM: Rewriting clause for ~p~n", [ModuleName]),
    
    %% Always add context extraction as first statement
    ContextExtraction = {match, Line, {var, Line, 'Context'}, 
                        {call,Line,{remote,Line,{atom,Line,maps},{atom,Line,get}},
                         [{atom,Line,context},{var,Line,'State'},{atom,Line,undefined}]}},
    
    %% Transform body based on module type
    NewBody = case is_client_module(ModuleName) of
        true ->
            io:format("PARSE_TRANSFORM: CLIENT - transforming with tuple handling~n"),
            transform_client_body_map(Body);
        false ->
            transform_regular_body_map(Body, ModuleName)
    end,
    
    {clause, Line, Patterns, Guards, [ContextExtraction | NewBody]}.

transform_client_body_map(Body) ->
    %% Check if there are any gen_server:call expressions in the body
    case has_gen_server_call(Body) of
        true ->
            %% Has wrapper calls - transform everything
            transform_client_body_map_with_context(Body, []);
        false ->
            %% No wrapper calls - return body unchanged
            Body
    end.

transform_client_body_map_with_context([], Acc) ->
    lists:reverse(Acc);
transform_client_body_map_with_context([Expr | Rest], Acc) ->
    case Expr of
        %% Pattern: Variable = gen_server:call(...)
        {match, _MLine, {var, VLine, VarName}, 
         {call, CLine, {remote, _RLine, {atom,_,gen_server}, {atom,_,call}}, [Target, Msg]}} ->
            TupleMatch = {match, VLine,
                         {tuple, VLine, [{var, VLine, VarName}, {var, VLine, 'NewContext'}]},
                         {call, CLine, {remote, CLine, {atom,CLine,simple_wrapper}, {atom,CLine,call}},
                          [Target, Msg, {var, CLine, 'Context'}, {atom, CLine, client}]}},
            transform_client_body_map_with_context(Rest, [TupleMatch | Acc]);
        
        %% Pattern: {reply, _, _} - Transform to include context update
        {tuple, Line, [{atom, _, reply}, ReplyVar, StateVar]} ->
            NewStateTuple = {tuple, Line, [
                {atom, Line, reply},
                ReplyVar,
                {call,Line,{remote,Line,{atom,Line,maps},{atom,Line,put}},
                 [{atom,Line,context},{var,Line,'NewContext'},StateVar]}
            ]},
            transform_client_body_map_with_context(Rest, [NewStateTuple | Acc]);
        
        %% Other expressions - keep unchanged
        _ ->
            transform_client_body_map_with_context(Rest, [Expr | Acc])
    end.

transform_regular_body_map(Body, ModuleName) ->
    lists:map(fun(Expr) -> transform_expr_map(Expr, ModuleName) end, Body).

transform_expr_map(Expr, ModuleName) ->
    case Expr of
        %% Direct gen_server:call - transform to simple_wrapper:call
        {call, CLine, {remote, RLine, {atom,_,gen_server}, {atom,_,call}}, [Target, Msg]} ->
            {call, CLine, {remote, RLine, {atom,RLine,simple_wrapper}, {atom,RLine,call}},
             [Target, Msg, {var, CLine, 'Context'}, {atom, CLine, ModuleName}]};
        
        %% Assignment: Var = gen_server:call(...) - ONLY for CLIENT modules use tuple destructuring
        {match, MLine, Var, {call, CLine, {remote, RLine, {atom,_,gen_server}, {atom,_,call}}, [Target, Msg]}} ->
            WrapperCall = {call, CLine, {remote, RLine, {atom,RLine,simple_wrapper}, {atom,RLine,call}},
                          [Target, Msg, {var, CLine, 'Context'}, {atom, CLine, ModuleName}]},
            case is_client_module(ModuleName) of
                true ->
                    %% For client: {Result, NewContext} = simple_wrapper:call(...)
                    TuplePattern = {tuple, MLine, [Var, {var, MLine, '_NewContext'}]},
                    {match, MLine, TuplePattern, WrapperCall};
                false ->
                    %% For non-client: Result = simple_wrapper:call(...) (keep original pattern)
                    {match, MLine, Var, WrapperCall}
            end;
        
        %% Handle all function calls recursively (same pattern as record version)
        {call, Line, Fun, Args} ->
            NewFun = transform_expr_map(Fun, ModuleName),
            NewArgs = lists:map(fun(Arg) -> transform_expr_map(Arg, ModuleName) end, Args),
            {call, Line, NewFun, NewArgs};
        
        %% Handle spawn calls specially
        {call, Line, {atom, _, spawn}, [Fun]} ->
            {call, Line, {atom, Line, spawn}, [transform_expr_map(Fun, ModuleName)]};
        
        %% Handle spawn/3 calls
        {call, Line, {atom, _, spawn}, [Module, Function, Args]} ->
            NewArgs = lists:map(fun(Arg) -> transform_expr_map(Arg, ModuleName) end, Args),
            {call, Line, {atom, Line, spawn}, [Module, Function, NewArgs]};
        
        %% Handle other compound expressions recursively (same as record version)
        {match, Line, Pattern, SubExpr} ->
            {match, Line, Pattern, transform_expr_map(SubExpr, ModuleName)};
        
        %% Handle try blocks with ALL possible structures
        {'try', Line, TryBody, CatchExpr, CatchClauses, AfterBody} ->
            NewTryBody = lists:map(fun(E) -> transform_expr_map(E, ModuleName) end, TryBody),
            NewCatchClauses = lists:map(fun({clause, CLine, Patterns, Guards, ClauseBody}) ->
                {clause, CLine, Patterns, Guards, lists:map(fun(E) -> transform_expr_map(E, ModuleName) end, ClauseBody)}
            end, CatchClauses),
            NewAfterBody = case AfterBody of
                [] -> [];
                _ -> lists:map(fun(E) -> transform_expr_map(E, ModuleName) end, AfterBody)
            end,
            {'try', Line, NewTryBody, CatchExpr, NewCatchClauses, NewAfterBody};
        
        %% Handle case expressions
        {'case', Line, CaseExpr, CaseClauses} ->
            NewCaseExpr = transform_expr_map(CaseExpr, ModuleName),
            NewCaseClauses = lists:map(fun({clause, CLine, Patterns, Guards, ClauseBody}) ->
                {clause, CLine, Patterns, Guards, lists:map(fun(E) -> transform_expr_map(E, ModuleName) end, ClauseBody)}
            end, CaseClauses),
            {'case', Line, NewCaseExpr, NewCaseClauses};
        
        %% Handle if expressions
        {'if', Line, IfClauses} ->
            NewIfClauses = lists:map(fun({clause, CLine, [], Guards, ClauseBody}) ->
                {clause, CLine, [], Guards, lists:map(fun(E) -> transform_expr_map(E, ModuleName) end, ClauseBody)}
            end, IfClauses),
            {'if', Line, NewIfClauses};
        
        %% Handle receive expressions
        {'receive', Line, ReceiveClauses} ->
            NewReceiveClauses = lists:map(fun({clause, CLine, Patterns, Guards, ClauseBody}) ->
                {clause, CLine, Patterns, Guards, lists:map(fun(E) -> transform_expr_map(E, ModuleName) end, ClauseBody)}
            end, ReceiveClauses),
            {'receive', Line, NewReceiveClauses};
        
        %% Handle receive with timeout
        {'receive', Line, ReceiveClauses, Timeout, TimeoutBody} ->
            NewReceiveClauses = lists:map(fun({clause, CLine, Patterns, Guards, ClauseBody}) ->
                {clause, CLine, Patterns, Guards, lists:map(fun(E) -> transform_expr_map(E, ModuleName) end, ClauseBody)}
            end, ReceiveClauses),
            NewTimeout = transform_expr_map(Timeout, ModuleName),
            NewTimeoutBody = lists:map(fun(E) -> transform_expr_map(E, ModuleName) end, TimeoutBody),
            {'receive', Line, NewReceiveClauses, NewTimeout, NewTimeoutBody};
        
        %% Handle fun expressions
        {'fun', Line, {clauses, FunClauses}} ->
            NewFunClauses = lists:map(fun({clause, CLine, Patterns, Guards, ClauseBody}) ->
                {clause, CLine, Patterns, Guards, lists:map(fun(E) -> transform_expr_map(E, ModuleName) end, ClauseBody)}
            end, FunClauses),
            {'fun', Line, {clauses, NewFunClauses}};
        
        %% Handle list comprehensions
        {lc, Line, Template, Qualifiers} ->
            NewTemplate = transform_expr_map(Template, ModuleName),
            NewQualifiers = lists:map(fun
                ({generate, QLine, Pattern, QExpr}) ->
                    {generate, QLine, Pattern, transform_expr_map(QExpr, ModuleName)};
                ({b_generate, QLine, Pattern, QExpr}) ->
                    {b_generate, QLine, Pattern, transform_expr_map(QExpr, ModuleName)};
                (QExpr) when is_tuple(QExpr) ->
                    transform_expr_map(QExpr, ModuleName);
                (Other) ->
                    Other
            end, Qualifiers),
            {lc, Line, NewTemplate, NewQualifiers};
        
        %% Handle binary comprehensions
        {bc, Line, Template, Qualifiers} ->
            NewTemplate = transform_expr_map(Template, ModuleName),
            NewQualifiers = lists:map(fun
                ({generate, QLine, Pattern, QExpr}) ->
                    {generate, QLine, Pattern, transform_expr_map(QExpr, ModuleName)};
                ({b_generate, QLine, Pattern, QExpr}) ->
                    {b_generate, QLine, Pattern, transform_expr_map(QExpr, ModuleName)};
                (QExpr) when is_tuple(QExpr) ->
                    transform_expr_map(QExpr, ModuleName);
                (Other) ->
                    Other
            end, Qualifiers),
            {bc, Line, NewTemplate, NewQualifiers};
        
        %% Handle tuples
        {tuple, Line, Elements} ->
            NewElements = lists:map(fun(Elem) -> transform_expr_map(Elem, ModuleName) end, Elements),
            {tuple, Line, NewElements};
        
        %% Handle lists
        {cons, Line, Head, Tail} ->
            NewHead = transform_expr_map(Head, ModuleName),
            NewTail = transform_expr_map(Tail, ModuleName),
            {cons, Line, NewHead, NewTail};
        
        %% Handle blocks
        {block, Line, BlockBody} ->
            NewBlockBody = lists:map(fun(E) -> transform_expr_map(E, ModuleName) end, BlockBody),
            {block, Line, NewBlockBody};
        
        %% Handle catch expressions
        {'catch', Line, CatchExpr} ->
            {'catch', Line, transform_expr_map(CatchExpr, ModuleName)};
        
        %% Default case - return as is
        _ ->
            Expr
    end.