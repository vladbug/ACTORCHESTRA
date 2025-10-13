-module(test_context_preservation).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

-export([start_link/0, run_context_test/0, run_multiple_context_test/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    context_map = #{}, % maps context_id -> {original_value, trace_path}
    test_results = []
}).

%%% API Functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Single context preservation test
run_context_test() ->
    Context = make_ref(),
    TestNumber = 42,
    
    io:format("~n=== CONTEXT PRESERVATION TEST ===~n"),
    io:format("Test Context: ~p~n", [Context]),
    io:format("Test Number: ~p~n", [TestNumber]),
    io:format("Expected final result: ~p~n", [(TestNumber + 10) * 2]),
    
    %% Start all necessary processes
    start_system(),
    
    %% Start our test tracker
    start_link(),
    
    %% Inject context tracking
    gen_server:call(?MODULE, {track_context, Context, TestNumber}),
    
    %% Create a client and test the flow
    {ok, ClientPid} = client:start_link(test_client),
    
    %% Connect client
    ok = client:connect(test_client),
    
    %% Send number with context injection through wrapper if available
    Result = case whereis(wrapper) of
        undefined ->
            %% Direct call without wrapper
            client:send_number(test_client, TestNumber);
        _WrapperPid ->
            %% Use wrapper for context propagation
            wrapper:call(test_client, {send_number, TestNumber}, Context)
    end,
    
    io:format("Final result: ~p~n", [Result]),
    
    %% Wait a bit for all async operations to complete
    timer:sleep(2000),
    
    %% Check context preservation results
    ContextResults = gen_server:call(?MODULE, get_context_results),
    
    io:format("~n=== CONTEXT TRACKING RESULTS ===~n"),
    case ContextResults of
        [] ->
            io:format("WARNING: No context tracking data collected!~n"),
            io:format("This might indicate context is not being properly propagated.~n");
        _ ->
            lists:foreach(fun({Ctx, {OrigValue, TracePath}}) ->
                io:format("Context ~p:~n", [Ctx]),
                io:format("  Original Value: ~p~n", [OrigValue]),
                io:format("  Trace Path: ~p~n", [TracePath])
            end, ContextResults)
    end,
    
    %% Verify the computation was correct
    ExpectedResult = (TestNumber + 10) * 2,
    case Result of
        {ok, ExpectedResult} ->
            io:format("✓ Computation PASSED: Got expected result ~p~n", [ExpectedResult]);
        {ok, OtherResult} ->
            io:format("✗ Computation FAILED: Expected ~p, got ~p~n", [ExpectedResult, OtherResult]);
        Error ->
            io:format("✗ Computation ERROR: ~p~n", [Error])
    end,
    
    %% Cleanup
    client:disconnect(test_client),
    
    io:format("~n=== TEST COMPLETE ===~n"),
    ok.

%% Multiple concurrent context preservation test
run_multiple_context_test() ->
    NumTests = 5,
    TestNumbers = lists:seq(10, 10 + NumTests - 1),
    
    io:format("~n=== MULTIPLE CONCURRENT CONTEXT TEST ===~n"),
    io:format("Running ~p concurrent tests with numbers: ~p~n", [NumTests, TestNumbers]),
    
    %% Start system
    start_system(),
    start_link(),
    
    %% Create contexts and start concurrent tests
    Contexts = [make_ref() || _ <- TestNumbers],
    ContextNumberPairs = lists:zip(Contexts, TestNumbers),
    
    %% Track all contexts
    lists:foreach(fun({Context, Number}) ->
        gen_server:call(?MODULE, {track_context, Context, Number})
    end, ContextNumberPairs),
    
    %% Start multiple clients and run tests concurrently
    ClientPids = lists:map(fun(N) ->
        ClientName = list_to_atom("test_client_" ++ integer_to_list(N)),
        {ok, Pid} = client:start_link(ClientName),
        ok = client:connect(ClientName),
        {ClientName, Pid}
    end, lists:seq(1, NumTests)),
    
    %% Launch all tests concurrently
    TestPids = lists:map(fun({{Context, Number}, {ClientName, _ClientPid}}) ->
        spawn(fun() ->
            io:format("Starting concurrent test: Context ~p, Number ~p, Client ~p~n", 
                     [Context, Number, ClientName]),
            
            Result = case whereis(wrapper) of
                undefined ->
                    client:send_number(ClientName, Number);
                _WrapperPid ->
                    wrapper:call(ClientName, {send_number, Number}, Context)
            end,
            
            Expected = (Number + 10) * 2,
            case Result of
                {ok, Expected} ->
                    io:format("✓ Test ~p PASSED: ~p -> ~p~n", [Number, Number, Expected]);
                {ok, Other} ->
                    io:format("✗ Test ~p FAILED: Expected ~p, got ~p~n", [Number, Expected, Other]);
                Error ->
                    io:format("✗ Test ~p ERROR: ~p~n", [Number, Error])
            end
        end)
    end, lists:zip(ContextNumberPairs, ClientPids)),
    
    %% Wait for all tests to complete
    lists:foreach(fun(Pid) ->
        Ref = monitor(process, Pid),
        receive
            {'DOWN', Ref, process, Pid, _Reason} -> ok
        after 10000 ->
            io:format("WARNING: Test process ~p timed out~n", [Pid])
        end
    end, TestPids),
    
    %% Wait a bit more for all async operations
    timer:sleep(3000),
    
    %% Check results
    ContextResults = gen_server:call(?MODULE, get_context_results),
    
    io:format("~n=== CONCURRENT CONTEXT TRACKING RESULTS ===~n"),
    io:format("Tracked ~p contexts out of ~p tests~n", [length(ContextResults), NumTests]),
    
    lists:foreach(fun({Ctx, {OrigValue, TracePath}}) ->
        io:format("Context ~p (value ~p): ~p~n", [Ctx, OrigValue, TracePath])
    end, ContextResults),
    
    %% Cleanup clients
    lists:foreach(fun({ClientName, _Pid}) ->
        client:disconnect(ClientName)
    end, ClientPids),
    
    io:format("~n=== CONCURRENT TEST COMPLETE ===~n"),
    ok.

%%% Internal functions

start_system() ->
    %% Start the system components if they're not already running
    case whereis(slow_central_server) of
        undefined ->
            io:format("Starting slow_central_server...~n"),
            {ok, _} = slow_central_server:start_link();
        _ ->
            io:format("slow_central_server already running~n")
    end,
    
    case whereis(wrapper) of
        undefined ->
            io:format("Starting wrapper...~n"),
            {ok, _} = wrapper:start_link();
        _ ->
            io:format("wrapper already running~n")
    end,
    
    ok.

%%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call({track_context, Context, OriginalValue}, _From, State) ->
    NewMap = maps:put(Context, {OriginalValue, [started]}, State#state.context_map),
    {reply, ok, State#state{context_map = NewMap}};

handle_call({context, Context}, _From, State) ->
    %% This gets called by the context_injector when context is being propagated
    case maps:get(Context, State#state.context_map, undefined) of
        undefined ->
            {reply, ok, State};
        {OrigValue, TracePath} ->
            NewTracePath = TracePath ++ [self()],
            NewMap = maps:put(Context, {OrigValue, NewTracePath}, State#state.context_map),
            io:format("Context ~p traced through ~p~n", [Context, self()]),
            {reply, ok, State#state{context_map = NewMap}}
    end;

handle_call(get_context_results, _From, State) ->
    Results = maps:to_list(State#state.context_map),
    {reply, Results, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
