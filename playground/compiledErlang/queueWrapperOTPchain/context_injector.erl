-module(context_injector).
-export([parse_transform/2]).

%% Entry point: transform only gen_server modules
parse_transform(Forms, _Opts) ->
    case is_gen_server(Forms) of
        true ->
            %% 0) Ensure state record gains a context field (only if not present)
            Forms0 = inject_context_record(Forms),
            %% 1) Inject the context clause at the top of handle_call/3
            Forms1 = inject_context_handler(Forms0),
            %% 2) Rewrite gen_server:call/2 into wrapper:call/3
            rewrite_process_calls(Forms1);
        false ->
            Forms
    end.

%%───────────────────────────────────────────
%% Detect gen_server behaviour attribute
is_gen_server(Forms) ->
    lists:any(fun
        ({attribute,_,behaviour,gen_server}) -> true;
        ({attribute,_,behavior,genserver}) -> true;
        (_) -> false
     end, Forms).

%%───────────────────────────────────────────
%% 0) Inject context field into -record(state, {...}) if missing
inject_context_record(Forms) ->
    lists:map(fun
        ( {attribute, Line, record, {state, Fields}} = RecAttr) ->
            FieldNames = [ Name || {Name, _Default} <- Fields ],
            case lists:member(context, FieldNames) of
                true ->
                    RecAttr;
                false ->
                    %% Append context = undefined at end of record
                    NewFields = Fields ++ [{context, undefined}],
                    {attribute, Line, record, {state, NewFields}}
            end;
        (Other) ->
            Other
    end, Forms).

%%───────────────────────────────────────────
%% 1) Inject a context handler clause into handle_call/3
inject_context_handler(Forms) ->
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            %% Create clause: handle_call({context, Context}, _From, State) ->
            CtxPat   = {tuple, Line, [{atom,Line,context}, {var,Line,'Context'}]},
            FromPat  = {var, Line, '_From'},
            StatePat = {var, Line, 'State'},
            %% Reply ok and update state with context
            ReplyExpr =
              {tuple,Line,
                [ {atom,Line,reply},
                  {atom,Line,ok},
                  {call,Line,
                    {remote,Line,{atom,Line,maps},{atom,Line,put}},
                    [{atom,Line,context}, {var,Line,'Context'}, {var,Line,'State'}]
                  }
                ]
              },
            CtxClause =
              {clause, Line, [CtxPat, FromPat, StatePat], [], [ReplyExpr]},
            {function, Line, handle_call, 3, [CtxClause|Clauses]};
        (Other) ->
            Other
    end, Forms).

%%───────────────────────────────────────────
%% 2) Rewrite gen_server:call/2 to wrapper:call/3 with context
rewrite_process_calls(Forms) ->
    lists:map(fun
        ({function, Line, handle_call, 3, Clauses}) ->
            NewClauses = [ rewrite_clause(Line, Clause) || Clause <- Clauses ],
            {function, Line, handle_call, 3, NewClauses};
        (Other) ->
            Other
    end, Forms).

rewrite_clause(_Line, {clause, CLine, [Req, From, State], Guards, Body}) ->
    NewBody = [ rewrite_expr(E) || E <- Body ],
    {clause, CLine, [Req, From, State], Guards, NewBody};
rewrite_clause(_, Other) ->
    Other.

rewrite_expr({call, CLine, {remote, RLine, {atom,_,gen_server}, {atom,_,call}}, [Target, Msg]}) ->
    %% Build maps:get(context, State, undefined)
    CtxExpr =
      {call, CLine,
        {remote, CLine, {atom,CLine,maps}, {atom,CLine,get}},
        [{atom,CLine,context}, {var,CLine,'State'}, {atom,CLine,undefined}]
      },
    %% Replace with wrapper:call(Target, Msg, Context)
    {call, CLine,
      {remote, RLine, {atom,RLine,wrapper}, {atom,RLine,call}},
      [Target, Msg, CtxExpr]
    };
rewrite_expr({call, CLine, Fun, Args}) when is_list(Args) ->
    {call, CLine, Fun, [rewrite_expr(A) || A <- Args]};
rewrite_expr({tuple, L, Es}) ->
    {tuple, L, [rewrite_expr(E) || E <- Es]};
rewrite_expr({cons, L, H, T}) ->
    {cons, L, rewrite_expr(H), rewrite_expr(T)};
rewrite_expr({'case', L, E, Clauses}) ->
    { 'case', L, rewrite_expr(E), [ rewrite_case(L, C) || C <- Clauses ] };
rewrite_expr({match, L, Left, Right}) ->
    {match, L, Left, rewrite_expr(Right)};
rewrite_expr(Other) ->
    Other.

rewrite_case(_Line, {clause, CLine, P, G, Body}) ->
    {clause, CLine, P, G, [rewrite_expr(E) || E <- Body] }.
