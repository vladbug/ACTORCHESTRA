-module(worker_a_test).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

%% API
-export([start_link/0, process_data/2, validate_input/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% This record will be modified by parse_transform to include context
-record(state, {
    processed_count = 0,
    last_result = undefined,
    %% Add debug fields
    call_history = []
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process_data(Data, Options) ->
    gen_server:call(?MODULE, {process_data, Data, Options}).

validate_input(Input) ->
    gen_server:call(?MODULE, {validate_input, Input}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    io:format("[worker_a] Starting~n"),
    {ok, #state{}}.

%% Note: The parse transform will inject a handler for {context, Context} here

%% These calls will be transformed by parse_transform to use wrapper:call/3
handle_call({process_data, Data, Options}, _From, State) ->
    io:format("[worker_a] Processing data: ~p with options: ~p, context: ~p~n", 
              [Data, Options, State#state.context]),
    
    %% Record this call
    CallRecord = {process_data, Data, State#state.context, erlang:system_time(microsecond)},
    
    %% This will be transformed to: wrapper:call(worker_b, {transform, Data}, Context)
    TransformResult = gen_server:call(worker_b_test, {transform, Data}),
    
    %% This will be transformed to: wrapper:call(worker_c, {validate, TransformResult, Options}, Context)
    ValidationResult = gen_server:call(worker_c_test, {validate, TransformResult, Options}),
    
    %% Local processing
    ProcessedCount = State#state.processed_count + 1,
    
    %% Include context in the result for verification
    FinalResult = {worker_a_result, [
        {transform_result, TransformResult},
        {validation_result, ValidationResult},
        {processed_count, ProcessedCount},
        {context, State#state.context},  %% Include context!
        {timestamp, erlang:system_time(microsecond)}
    ]},
    
    io:format("[worker_a] Final result with context ~p~n", [State#state.context]),
    
    NewState = State#state{
        processed_count = ProcessedCount,
        last_result = FinalResult,
        call_history = [CallRecord | State#state.call_history]
    },
    
    {reply, FinalResult, NewState};

handle_call({validate_input, Input}, _From, State) ->
    io:format("[worker_a] Validating input: ~p with context: ~p~n", 
              [Input, State#state.context]),
    
    %% Record this call
    CallRecord = {validate_input, Input, State#state.context, erlang:system_time(microsecond)},
    
    %% This will be transformed to: wrapper:call(worker_c, {check_format, Input}, Context)
    FormatCheck = gen_server:call(worker_c_test, {check_format, Input}),
    
    Result = case FormatCheck of
        {valid, Props} -> 
            {input_valid, [
                {input, Input},
                {format_check_result, Props},
                {context, State#state.context}
            ]};
        {invalid, Props} -> 
            {input_invalid, [
                {input, Input},
                {format_check_result, Props},
                {context, State#state.context}
            ]};
        %% Handle old format for backwards compatibility
        valid -> 
            {input_valid, [
                {input, Input},
                {context, State#state.context}
            ]};
        invalid -> 
            {input_invalid, [
                {input, Input},
                {context, State#state.context}
            ]}
    end,
    
    NewState = State#state{
        call_history = [CallRecord | State#state.call_history]
    },
    
    {reply, Result, NewState};

%% Debug handler to get call history
handle_call(get_call_history, _From, State) ->
    {reply, {call_history, lists:reverse(State#state.call_history)}, State};

%% Debug handler to get current context
handle_call(get_context, _From, State) ->
    {reply, {current_context, State#state.context}, State};

handle_call(Request, _From, State) ->
    io:format("[worker_a] Unknown request: ~p~n", [Request]),
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[worker_a] Terminating~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.