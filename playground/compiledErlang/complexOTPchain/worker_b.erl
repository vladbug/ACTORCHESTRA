-module(worker_b).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

%% API
-export([start_link/0, transform/1, batch_transform/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    transform_count = 0,
    cache = #{}
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

handle_call({transform, Data}, _From, State) ->
    io:format("[worker_b] Transforming data: ~p~n", [Data]),
    
    %% Simulate some processing time
    timer:sleep(50),
    
    %% Check cache first (this will be transformed to use wrapper:call/3)
    CacheKey = {cache_key, Data},
    CacheResult = gen_server:call(worker_c, {cache_lookup, CacheKey}),
    
    TransformedData = case CacheResult of
        {hit, CachedValue} ->
            io:format("[worker_b] Cache hit for ~p~n", [Data]),
            CachedValue;
        miss ->
            io:format("[worker_b] Cache miss, performing transformation~n"),
            %% Actual transformation logic
            Transformed = {transformed, Data, erlang:system_time()},
            
            %% Store in cache (this will be transformed to use wrapper:call/3)
            gen_server:call(worker_c, {cache_store, CacheKey, Transformed}),
            Transformed
    end,
    
    Count = State#state.transform_count + 1,
    NewState = State#state{transform_count = Count},
    
    Result = {worker_b_result, TransformedData, Count},
    io:format("[worker_b] Transform result: ~p~n", [Result]),
    
    {reply, Result, NewState};

handle_call({batch_transform, DataList}, _From, State) ->
    io:format("[worker_b] Batch transforming: ~p~n", [DataList]),
    
    %% Process each item through worker_c for validation
    %% This will be transformed to use wrapper:call/3
    Results = [begin
        ValidateResult = gen_server:call(worker_c, {pre_validate, Item}),
        case ValidateResult of
            valid -> {transformed, Item, batch};
            invalid -> {error, Item, invalid_input}
        end
    end || Item <- DataList],
    
    Count = State#state.transform_count + length(DataList),
    NewState = State#state{transform_count = Count},
    
    {reply, {batch_result, Results}, NewState};

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