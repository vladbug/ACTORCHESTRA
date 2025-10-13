-module(worker_c).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

%% API
-export([start_link/0, validate/2, check_format/1, cache_lookup/1, cache_store/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    validation_count = 0,
    cache = #{},
    stats = #{}
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

handle_call({validate, Data, Options}, _From, State) ->
    io:format("[worker_c] Validating data: ~p with options: ~p~n", [Data, Options]),
    
    %% Simulate validation processing
    timer:sleep(30),
    
    %% Complex validation that might call other workers
    %% This will be transformed to use wrapper:call/3 
    case Data of
        {worker_b_result, TransformedData, _Count} ->
            %% Validate the transformed data by checking with worker_b
            DoubleCheckResult = gen_server:call(worker_b, {transform, raw_validation_check}),
            
            ValidationResult = case {TransformedData, Options, DoubleCheckResult} of
                {{transformed, _, _}, #{strict := true}, _} ->
                    strict_validation_passed;
                {{transformed, _, _}, _, {worker_b_result, _, _}} ->
                    standard_validation_passed;
                _ ->
                    validation_failed
            end,
            
            Count = State#state.validation_count + 1,
            NewState = State#state{validation_count = Count},
            
            Result = {worker_c_validation, ValidationResult, Count},
            io:format("[worker_c] Validation result: ~p~n", [Result]),
            
            {reply, Result, NewState};
        
        _ ->
            {reply, {validation_error, unsupported_data_format}, State}
    end;

handle_call({check_format, Input}, _From, State) ->
    io:format("[worker_c] Checking format of: ~p~n", [Input]),
    
    Result = case Input of
        {client_data, _, _} -> valid;
        {batch_data, _} -> valid;
        _ when is_atom(Input) -> valid;
        _ when is_binary(Input) -> valid;
        _ -> invalid
    end,
    
    {reply, Result, State};

handle_call({pre_validate, Item}, _From, State) ->
    io:format("[worker_c] Pre-validating item: ~p~n", [Item]),
    
    Result = case Item of
        _ when is_integer(Item), Item > 0 -> valid;
        _ when is_atom(Item) -> valid;
        _ -> invalid
    end,
    
    {reply, Result, State};

handle_call({cache_lookup, Key}, _From, State) ->
    io:format("[worker_c] Cache lookup for: ~p~n", [Key]),
    
    Result = case maps:get(Key, State#state.cache, not_found) of
        not_found -> miss;
        Value -> {hit, Value}
    end,
    
    {reply, Result, State};

handle_call({cache_store, Key, Value}, _From, State) ->
    io:format("[worker_c] Storing in cache: ~p -> ~p~n", [Key, Value]),
    
    NewCache = maps:put(Key, Value, State#state.cache),
    NewState = State#state{cache = NewCache},
    
    {reply, ok, NewState};

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