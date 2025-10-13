-module(worker_c_test).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

%% API
-export([start_link/0, validate/2, check_format/1, cache_lookup/1, cache_store/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    validation_count = 0,
    cache = #{},
    stats = #{
        cache_hits => 0,
        cache_misses => 0,
        validations_passed => 0,
        validations_failed => 0
    },
    %% Add debug fields
    call_history = []
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

validate(Data, Options) ->
    gen_server:call(?MODULE, {validate, Data, Options}).

check_format(Input) ->
    gen_server:call(?MODULE, {check_format, Input}).

cache_lookup(Key) ->
    gen_server:call(?MODULE, {cache_lookup, Key}).

cache_store(Key, Value) ->
    gen_server:call(?MODULE, {cache_store, Key, Value}).

%%====================================================================
%% gen_server callbacks  
%%====================================================================

init([]) ->
    io:format("[worker_c] Starting~n"),
    {ok, #state{}}.

%% Note: The parse transform will inject a handler for {context, Context} here

handle_call({validate, Data, Options}, _From, State) ->
    io:format("[worker_c] Validating data: ~p with options: ~p, context: ~p~n", 
              [Data, Options, State#state.context]),
    
    %% Record this call
    CallRecord = {validate, Data, Options, State#state.context, erlang:system_time(microsecond)},
    
    %% Simulate validation processing
    timer:sleep(30),
    
    %% Simple validation without circular dependencies
    case Data of
        {worker_b_result, Props} ->
            %% Extract transformed data from worker_b result
            TransformedData = proplists:get_value(transformed_data, Props),
            BContext = proplists:get_value(context, Props),
            
            io:format("[worker_c] Received data from worker_b with context: ~p~n", [BContext]),
            
            %% Perform validation based on data and options
            ValidationResult = case {TransformedData, Options} of
                {{transformed, _, _}, #{strict := true}} ->
                    strict_validation_passed;
                {{transformed, _, _}, _} ->
                    standard_validation_passed;
                _ ->
                    validation_failed
            end,
            
            %% Update stats
            NewStats = case ValidationResult of
                validation_failed ->
                    maps:update_with(validations_failed, fun(V) -> V + 1 end, 1, State#state.stats);
                _ ->
                    maps:update_with(validations_passed, fun(V) -> V + 1 end, 1, State#state.stats)
            end,
            
            Count = State#state.validation_count + 1,
            NewState = State#state{
                validation_count = Count,
                call_history = [CallRecord | State#state.call_history],
                stats = NewStats
            },
            
            %% Include context in result
            Result = {worker_c_validation, [
                {validation_result, ValidationResult},
                {validation_count, Count},
                {received_from_b_context, BContext},
                {my_context, State#state.context},
                {timestamp, erlang:system_time(microsecond)}
            ]},
            
            io:format("[worker_c] Validation result with context ~p~n", [State#state.context]),
            
            {reply, Result, NewState};
        
        _ ->
            {reply, {validation_error, unsupported_data_format, 
                    [{context, State#state.context}]}, State}
    end;

handle_call({check_format, Input}, _From, State) ->
    io:format("[worker_c] Checking format of: ~p with context: ~p~n", 
              [Input, State#state.context]),
    
    %% Record this call
    CallRecord = {check_format, Input, State#state.context, erlang:system_time(microsecond)},
    
    {Result, Reason} = case Input of
        {client_data, _, _} -> {valid, client_data_format};
        {batch_data, _} -> {valid, batch_data_format};
        _ when is_atom(Input) -> {valid, atom_format};
        _ when is_binary(Input) -> {valid, binary_format};
        _ -> {invalid, unknown_format}
    end,
    
    NewState = State#state{
        call_history = [CallRecord | State#state.call_history]
    },
    
    %% Include context in format check result
    Reply = {Result, [
        {input_type, Reason},
        {context, State#state.context},
        {timestamp, erlang:system_time(microsecond)}
    ]},
    
    {reply, Reply, NewState};

handle_call({pre_validate, Item}, _From, State) ->
    io:format("[worker_c] Pre-validating item: ~p with context: ~p~n", 
              [Item, State#state.context]),
    
    {Result, Reason} = case Item of
        _ when is_integer(Item), Item > 0 -> {valid, positive_integer};
        _ when is_atom(Item) -> {valid, atom_type};
        _ -> {invalid, invalid_type}
    end,
    
    %% Include context in pre-validation result
    Reply = {Result, [
        {validation_reason, Reason},
        {context, State#state.context},
        {timestamp, erlang:system_time(microsecond)}
    ]},
    
    {reply, Reply, State};

handle_call({cache_lookup, Key}, _From, State) ->
    io:format("[worker_c] Cache lookup for: ~p with context: ~p~n", 
              [Key, State#state.context]),
    
    {Result, NewStats} = case maps:get(Key, State#state.cache, not_found) of
        not_found -> 
            Stats = maps:update_with(cache_misses, fun(V) -> V + 1 end, 1, State#state.stats),
            {miss, Stats};
        {Value, CacheContext} -> 
            Stats = maps:update_with(cache_hits, fun(V) -> V + 1 end, 1, State#state.stats),
            %% Include the context that stored this cache entry
            {{hit, Value, [
                {cached_by_context, CacheContext},
                {current_context, State#state.context},
                {timestamp, erlang:system_time(microsecond)}
            ]}, Stats}
    end,
    
    NewState = State#state{stats = NewStats},
    {reply, Result, NewState};

handle_call({cache_store, Key, Value}, _From, State) ->
    io:format("[worker_c] Storing in cache: ~p -> ~p with context: ~p~n", 
              [Key, Value, State#state.context]),
    
    %% Store value with context information
    NewCache = maps:put(Key, {Value, State#state.context}, State#state.cache),
    NewState = State#state{cache = NewCache},
    
    {reply, ok, NewState};

%% Debug handler to get call history
handle_call(get_call_history, _From, State) ->
    {reply, {call_history, lists:reverse(State#state.call_history)}, State};

%% Debug handler to get current context
handle_call(get_context, _From, State) ->
    {reply, {current_context, State#state.context}, State};

%% Debug handler to inspect cache
handle_call(get_cache_info, _From, State) ->
    CacheInfo = maps:fold(fun(K, {V, Ctx}, Acc) ->
        [{K, V, Ctx} | Acc]
    end, [], State#state.cache),
    {reply, {cache_info, CacheInfo}, State};

%% Debug handler to get statistics
handle_call(get_stats, _From, State) ->
    {reply, {stats, State#state.stats}, State};

handle_call(Request, _From, State) ->
    io:format("[worker_c] Unknown request: ~p~n", [Request]),
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[worker_c] Terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.