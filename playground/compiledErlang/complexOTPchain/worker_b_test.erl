-module(worker_b_test).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

%% API
-export([start_link/0, transform/1, batch_transform/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    transform_count = 0,
    %% Remove unused cache field - worker_c handles caching
    %% Add debug fields
    call_history = []
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

transform(Data) ->
    gen_server:call(?MODULE, {transform, Data}).

batch_transform(DataList) ->
    gen_server:call(?MODULE, {batch_transform, DataList}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[worker_b] Starting~n"),
    {ok, #state{}}.

%% Note: The parse transform will inject a handler for {context, Context} here

handle_call({transform, Data}, _From, State) ->
    io:format("[worker_b] Transforming data: ~p with context: ~p~n", 
              [Data, State#state.context]),
    
    %% Record this call
    CallRecord = {transform, Data, State#state.context, erlang:system_time(microsecond)},
    
    %% Simulate some processing time
    timer:sleep(50),
    
    %% Check cache first (this will be transformed to use wrapper:call/3)
    CacheKey = {cache_key, Data},
    CacheResult = gen_server:call(worker_c_test, {cache_lookup, CacheKey}),
    
    {TransformedData, CacheStatus} = case CacheResult of
        {hit, CachedValue, CacheProps} ->
            io:format("[worker_b] Cache hit for ~p~n", [Data]),
            %% Extract context from cache result if available
            CacheContext = proplists:get_value(context, CacheProps, unknown),
            io:format("[worker_b] Cache was from context: ~p~n", [CacheContext]),
            {CachedValue, cache_hit};
        miss ->
            io:format("[worker_b] Cache miss, performing transformation~n"),
            %% Actual transformation logic
            Transformed = {transformed, Data, erlang:system_time()},
            
            %% Store in cache (this will be transformed to use wrapper:call/3)
            gen_server:call(worker_c_test, {cache_store, CacheKey, Transformed}),
            {Transformed, cache_miss}
    end,
    
    Count = State#state.transform_count + 1,
    NewState = State#state{
        transform_count = Count,
        call_history = [CallRecord | State#state.call_history]
    },
    
    %% Include context in the result
    Result = {worker_b_result, [
        {transformed_data, TransformedData},
        {transform_count, Count},
        {cache_status, CacheStatus},
        {context, State#state.context},  %% Include context!
        {timestamp, erlang:system_time(microsecond)}
    ]},
    
    io:format("[worker_b] Transform result with context ~p~n", [State#state.context]),
    
    {reply, Result, NewState};

handle_call({batch_transform, DataList}, _From, State) ->
    io:format("[worker_b] Batch transforming: ~p with context: ~p~n", 
              [DataList, State#state.context]),
    
    %% Record this call
    CallRecord = {batch_transform, DataList, State#state.context, erlang:system_time(microsecond)},
    
    %% Process each item through worker_c for validation
    %% This will be transformed to use wrapper:call/3
    Results = [begin
        ValidateResult = gen_server:call(worker_c_test, {pre_validate, Item}),
        case ValidateResult of
            {valid, ValidProps} -> 
                {transformed, [
                    {item, Item},
                    {type, batch},
                    {validation_props, ValidProps}
                ]};
            {invalid, InvalidProps} -> 
                {error, [
                    {item, Item},
                    {reason, invalid_input},
                    {validation_props, InvalidProps}
                ]};
            %% Handle old format
            valid -> {transformed, Item, batch};
            invalid -> {error, Item, invalid_input}
        end
    end || Item <- DataList],
    
    Count = State#state.transform_count + length(DataList),
    NewState = State#state{
        transform_count = Count,
        call_history = [CallRecord | State#state.call_history]
    },
    
    %% Include context in batch result
    Result = {batch_result, [
        {results, Results},
        {total_count, Count},
        {context, State#state.context},  %% Include context!
        {timestamp, erlang:system_time(microsecond)}
    ]},
    
    {reply, Result, NewState};

%% Debug handler to get call history
handle_call(get_call_history, _From, State) ->
    {reply, {call_history, lists:reverse(State#state.call_history)}, State};

%% Debug handler to get current context
handle_call(get_context, _From, State) ->
    {reply, {current_context, State#state.context}, State};

handle_call(Request, _From, State) ->
    io:format("[worker_b] Unknown request: ~p~n", [Request]),
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[worker_b] Terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.