-module(comprehensive_context_test).
-export([run_all_tests/0, run_chain_verification/0, run_context_consistency_test/0, 
         run_cache_context_test/0, analyze_results/1]).

%%====================================================================
%% Main Test Suite
%%====================================================================

run_all_tests() ->
    io:format("~n=== COMPREHENSIVE CONTEXT PROPAGATION TEST ===~n"),
    
    ensure_system_started(),
    
    %% Test 1: Verify context flows through entire chain
    run_chain_verification(),
    timer:sleep(1000),
    
    %% Test 2: Verify context consistency across multiple paths
    run_context_consistency_test(),
    timer:sleep(1000),
    
    %% Test 3: Verify context in cache operations
    run_cache_context_test(),
    
    io:format("~n=== ALL TESTS COMPLETE ===~n"),
    ok.

%%====================================================================
%% Test 1: Chain Verification
%%====================================================================

run_chain_verification() ->
    io:format("~n--- TEST 1: Context Chain Verification ---~n"),
    
    %% Test 1.1: Simple chain A -> B -> C
    io:format("~nTest 1.1: Simple chain A -> B -> C~n"),
    
    Context1 = make_ref(),
    io:format("Starting chain with context: ~p~n", [Context1]),
    
    %% Call through the entire chain
    Result = wrapper:call(worker_a_test, {process_data, {chain_test, 1}, #{strict => true}}, Context1),
    
    %% Analyze the result
    case Result of
        {worker_a_result, Props} ->
            %% Extract contexts from each worker
            AContext = proplists:get_value(context, Props),
            
            %% Get B's context from transform result
            {worker_b_result, BProps} = proplists:get_value(transform_result, Props),
            BContext = proplists:get_value(context, BProps),
            
            %% Get C's context from validation result
            {worker_c_validation, CProps} = proplists:get_value(validation_result, Props),
            CContext = proplists:get_value(my_context, CProps),
            BContextSeenByC = proplists:get_value(received_from_b_context, CProps),
            
            io:format("Context propagation:~n"),
            io:format("  Original: ~p~n", [Context1]),
            io:format("  Worker A: ~p~n", [AContext]),
            io:format("  Worker B: ~p~n", [BContext]),
            io:format("  Worker C: ~p~n", [CContext]),
            io:format("  B context seen by C: ~p~n", [BContextSeenByC]),
            
            %% Verify all contexts match
            if
                Context1 =:= AContext andalso
                AContext =:= BContext andalso
                BContext =:= CContext andalso
                BContext =:= BContextSeenByC ->
                    io:format("✅ Context correctly propagated through entire chain!~n");
                true ->
                    io:format("❌ Context mismatch in chain!~n")
            end;
        Error ->
            io:format("❌ Unexpected result format: ~p~n", [Error])
    end,
    
    %% Test 1.2: Alternative path A -> C
    io:format("~nTest 1.2: Alternative path A -> C~n"),
    
    Context2 = make_ref(),
    Result2 = wrapper:call(worker_a_test, {validate_input, {client_data, test, 123}}, Context2),
    
    case Result2 of
        {input_valid, Props2} ->
            AContext2 = proplists:get_value(context, Props2),
            {valid, CProps2} = proplists:get_value(format_check_result, Props2),
            CContext2 = proplists:get_value(context, CProps2),
            
            io:format("Context in A -> C path:~n"),
            io:format("  Original: ~p~n", [Context2]),
            io:format("  Worker A: ~p~n", [AContext2]),
            io:format("  Worker C: ~p~n", [CContext2]),
            
            if
                Context2 =:= AContext2 andalso AContext2 =:= CContext2 ->
                    io:format("✅ Context correctly propagated in A -> C path!~n");
                true ->
                    io:format("❌ Context mismatch in A -> C path!~n")
            end;
        _ ->
            io:format("❌ Unexpected result in A -> C test~n")
    end.

%%====================================================================
%% Test 2: Context Consistency
%%====================================================================

run_context_consistency_test() ->
    io:format("~n--- TEST 2: Context Consistency Test ---~n"),
    
    %% Launch multiple concurrent requests with different contexts
    Contexts = [make_ref() || _ <- lists:seq(1, 5)],
    
    io:format("Testing with ~p different contexts concurrently~n", [length(Contexts)]),
    
    %% Start all requests
    Self = self(),
    Pids = lists:map(fun({N, Context}) ->
        spawn(fun() ->
            %% Each client makes multiple calls with same context
            R1 = wrapper:call(worker_a_test, {process_data, {consistency, N, 1}, #{}}, Context),
            timer:sleep(50),
            R2 = wrapper:call(worker_a_test, {validate_input, {batch_data, [N]}}, Context),
            timer:sleep(50),
            R3 = wrapper:call(worker_b_test, {transform, {consistency, N, 2}}, Context),

            Self ! {consistency_results, N, Context, [R1, R2, R3]}
        end)
    end, lists:zip(lists:seq(1, 5), Contexts)),
    
    %% Collect results
    Results = lists:map(fun(_) ->
        receive
            {consistency_results, N, Context, Rs} -> {N, Context, Rs}
        after 5000 ->
            {error, timeout}
        end
    end, Pids),
    
    %% Analyze consistency
    lists:foreach(fun
        ({N, Context, [R1, R2, R3]}) ->
            io:format("~nClient ~p (Context: ~p):~n", [N, Context]),
            
            %% Extract contexts from all results
            Contexts = extract_all_contexts([R1, R2, R3]),
            UniqueContexts = lists:usort(Contexts),
            
            case UniqueContexts of
                [Context] ->
                    io:format("  ✅ All ~p operations used consistent context~n", [length(Contexts)]);
                [] ->
                    io:format("  ❌ No contexts found in results~n");
                Multiple ->
                    io:format("  ❌ Multiple contexts found: ~p~n", [Multiple])
            end;
        ({error, timeout}) ->
            io:format("  ❌ Client timed out~n")
    end, Results).

%%====================================================================
%% Test 3: Cache Context Test
%%====================================================================

run_cache_context_test() ->
    io:format("~n--- TEST 3: Cache Context Test ---~n"),
    
    %% Test that cached values remember their context
    CacheKey = {test_cache_key, erlang:unique_integer()},
    
    %% Store value with context 1
    Context1 = make_ref(),
    io:format("Storing cache value with context: ~p~n", [Context1]),
    
    %% Call B which will cache in C
    R1 = wrapper:call(worker_b_test, {transform, CacheKey}, Context1),
    
    %% Now retrieve with different context
    Context2 = make_ref(),
    io:format("Retrieving cache value with context: ~p~n", [Context2]),
    
    R2 = wrapper:call(worker_b_test, {transform, CacheKey}, Context2),
    
    %% Analyze cache behavior
    case {R1, R2} of
        {{worker_b_result, Props1}, {worker_b_result, Props2}} ->
            Status1 = proplists:get_value(cache_status, Props1),
            Status2 = proplists:get_value(cache_status, Props2),
            Ctx1 = proplists:get_value(context, Props1),
            Ctx2 = proplists:get_value(context, Props2),
            
            io:format("First call: cache ~p, context ~p~n", [Status1, Ctx1]),
            io:format("Second call: cache ~p, context ~p~n", [Status2, Ctx2]),
            
            if
                Status1 =:= cache_miss andalso Status2 =:= cache_hit ->
                    io:format("✅ Cache working correctly across contexts~n");
                true ->
                    io:format("❌ Unexpected cache behavior~n")
            end,
            
            if
                Ctx1 =:= Context1 andalso Ctx2 =:= Context2 ->
                    io:format("✅ Each call maintained its own context~n");
                true ->
                    io:format("❌ Context contamination detected~n")
            end;
        _ ->
            io:format("❌ Unexpected result format~n")
    end,
    
    %% Check cache internals
    {cache_info, CacheInfo} = gen_server:call(worker_c_test, get_cache_info),
    io:format("~nCache contents:~n"),
    lists:foreach(fun({K, V, Ctx}) ->
        io:format("  Key: ~p, Value: ~p, Stored by context: ~p~n", [K, V, Ctx])
    end, CacheInfo).

%%====================================================================
%% Helper Functions
%%====================================================================

%% Extract all contexts from nested result structures
extract_all_contexts(Results) ->
    lists:flatten(lists:map(fun extract_contexts_from_result/1, Results)).

extract_contexts_from_result({worker_a_result, Props}) ->
    Contexts = [proplists:get_value(context, Props)],
    
    %% Extract from nested results
    TransformResult = proplists:get_value(transform_result, Props),
    ValidationResult = proplists:get_value(validation_result, Props),
    
    Contexts ++ extract_contexts_from_result(TransformResult) ++ 
                extract_contexts_from_result(ValidationResult);

extract_contexts_from_result({worker_b_result, Props}) ->
    [proplists:get_value(context, Props)];

extract_contexts_from_result({worker_c_validation, Props}) ->
    [proplists:get_value(my_context, Props),
     proplists:get_value(received_from_b_context, Props)];

extract_contexts_from_result({input_valid, Props}) ->
    BaseContext = proplists:get_value(context, Props),
    case proplists:get_value(format_check_result, Props) of
        {_, CProps} -> [BaseContext, proplists:get_value(context, CProps)];
        _ -> [BaseContext]
    end;

extract_contexts_from_result({input_invalid, Props}) ->
    extract_contexts_from_result({input_valid, Props});

extract_contexts_from_result({batch_result, Props}) ->
    [proplists:get_value(context, Props)];

extract_contexts_from_result(_) ->
    [].

%% Analyze and report on test results
analyze_results(TestName) ->
    io:format("~n=== Analysis for ~s ===~n", [TestName]),
    
    %% Get call histories from all workers
    {call_history, AHistory} = gen_server:call(worker_a_test, get_call_history),
    {call_history, BHistory} = gen_server:call(worker_b_test, get_call_history),
    {call_history, CHistory} = gen_server:call(worker_c_test, get_call_history),

    io:format("~nCall histories:~n"),
    io:format("Worker A: ~p calls~n", [length(AHistory)]),
    io:format("Worker B: ~p calls~n", [length(BHistory)]),
    io:format("Worker C: ~p calls~n", [length(CHistory)]),
    
    %% Analyze context distribution
    AllContexts = extract_contexts_from_histories([AHistory, BHistory, CHistory]),
    UniqueContexts = lists:usort(AllContexts),
    
    io:format("~nContext distribution:~n"),
    io:format("Total contexts seen: ~p~n", [length(UniqueContexts)]),
    
    %% Count occurrences of each context
    ContextCounts = lists:map(fun(Ctx) ->
        Count = length([C || C <- AllContexts, C =:= Ctx]),
        {Ctx, Count}
    end, UniqueContexts),
    
    lists:foreach(fun({Ctx, Count}) ->
        io:format("  Context ~p: ~p occurrences~n", [Ctx, Count])
    end, ContextCounts).

extract_contexts_from_histories(Histories) ->
    lists:flatten(lists:map(fun(History) ->
        lists:filtermap(fun
            ({_, _, Context, _}) when Context =/= undefined -> {true, Context};
            ({_, _, _, Context, _}) when Context =/= undefined -> {true, Context};
            (_) -> false
        end, History)
    end, Histories)).

%%====================================================================
%% System Setup
%%====================================================================

ensure_system_started() ->
    io:format("Starting system components...~n"),
    
    %% Start wrapper
    case wrapper:start_link() of
        {ok, _} -> io:format("✓ Wrapper started~n");
        {error, {already_started, _}} -> 
            %% Restart for clean state
            catch gen_server:stop(wrapper),
            timer:sleep(100),
            {ok, _} = wrapper:start_link(),
            io:format("✓ Wrapper restarted~n")
    end,
    
    %% Start workers (restart for clean state)
    Workers = [
        {worker_a_test, fun worker_a:start_link/0},
        {worker_b_test, fun worker_b:start_link/0},
        {worker_c_test, fun worker_c:start_link/0}
    ],
    
    lists:foreach(fun({Name, StartFun}) ->
        catch gen_server:stop(Name),
        timer:sleep(100),
        case StartFun() of
            {ok, _} -> io:format("✓ ~p started fresh~n", [Name]);
            Error -> io:format("✗ ~p failed to start: ~p~n", [Name, Error])
        end
    end, Workers),
    
    timer:sleep(200),
    ok.

%%====================================================================
%% Additional Advanced Tests
%%====================================================================

%% Test context propagation with errors
test_error_context_propagation() ->
    io:format("~n--- Testing Context Propagation with Errors ---~n"),
    
    Context = make_ref(),
    
    %% Cause an error by sending invalid data
    Result = wrapper:call(worker_a_test, {process_data, invalid_data_format, #{}}, Context),
    
    case Result of
        {error, _} ->
            io:format("✅ Error handled correctly~n"),
            %% Check if context was maintained even in error case
            {current_context, AContext} = gen_server:call(worker_a_test, get_context),
            if
                AContext =:= Context ->
                    io:format("✅ Context maintained even after error~n");
                true ->
                    io:format("❌ Context lost after error~n")
            end;
        _ ->
            io:format("❌ Expected error but got: ~p~n", [Result])
    end.

%% Test rapid context switching
test_rapid_context_switching() ->
    io:format("~n--- Testing Rapid Context Switching ---~n"),
    
    %% Make many rapid calls with different contexts
    Contexts = [make_ref() || _ <- lists:seq(1, 20)],
    
    %% Fire all requests as quickly as possible
    lists:foreach(fun(Context) ->
        spawn(fun() ->
            wrapper:call(worker_a_test, {validate_input, test_input}, Context)
        end)
    end, Contexts),
    
    timer:sleep(2000),
    
    %% Check final state
    {call_history, History} = gen_server:call(worker_a_test, get_call_history),
    HistoryContexts = [Ctx || {_, _, Ctx, _} <- History],
    
    %% All contexts should appear in history
    Missing = Contexts -- HistoryContexts,
    case Missing of
        [] ->
            io:format("✅ All ~p contexts were processed~n", [length(Contexts)]);
        _ ->
            io:format("❌ Missing contexts: ~p~n", [length(Missing)])
    end.

%%====================================================================
%% Usage Instructions  
%%====================================================================

%% To run the comprehensive context test:
%%
%% 1. Compile all modules:
%%    c(context_injector).
%%    c(wrapper).
%%    c(worker_a).  % The enhanced version with context in response
%%    c(worker_b).  % The enhanced version with context in response
%%    c(worker_c).  % The enhanced version with context in response
%%    c(comprehensive_context_test).
%%
%% 2. Run the full test suite:
%%    comprehensive_context_test:run_all_tests().
%%
%% Or run individual tests:
%%    comprehensive_context_test:run_chain_verification().
%%    comprehensive_context_test:run_context_consistency_test().
%%    comprehensive_context_test:run_cache_context_test().
%%
%% For detailed analysis after running tests:
%%    comprehensive_context_test:analyze_results("Test Run 1").
%%
%% The enhanced workers now include context in all their responses,
%% allowing complete verification of context propagation.