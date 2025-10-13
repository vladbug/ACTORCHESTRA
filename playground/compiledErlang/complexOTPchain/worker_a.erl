-module(worker_a).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

%% API
-export([start_link/0, process_data/2, validate_input/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% This record will be injected by parse_transform
%% -record(state, {context = undefined, ...}).

-record(state, {
    processed_count = 0,
    last_result = undefined
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


%% These calls will be transformed by parse_transform to use wrapper:call/3
handle_call({process_data, Data, Options}, _From, State) ->
    io:format("[worker_a] Processing data: ~p with options: ~p~n", [Data, Options]),
    
    %% This will be transformed to: wrapper:call(worker_b, {transform, Data}, Context)
    TransformResult = gen_server:call(worker_b, {transform, Data}),
    
    %% This will be transformed to: wrapper:call(worker_c, {validate, TransformResult, Options}, Context)
    ValidationResult = gen_server:call(worker_c, {validate, TransformResult, Options}),
    
    %% Local processing
    ProcessedCount = State#state.processed_count + 1,
    FinalResult = {worker_a_result, TransformResult, ValidationResult, ProcessedCount},
    
    io:format("[worker_a] Final result: ~p~n", [FinalResult]),
    
    NewState = State#state{
        processed_count = ProcessedCount,
        last_result = FinalResult
    },
    
    {reply, FinalResult, NewState};

handle_call({validate_input, Input}, _From, State) ->
    io:format("[worker_a] Validating input: ~p~n", [Input]),
    
    %% This will be transformed to: wrapper:call(worker_c, {check_format, Input}, Context)
    FormatCheck = gen_server:call(worker_c, {check_format, Input}),
    
    Result = case FormatCheck of
        valid -> {input_valid, Input};
        invalid -> {input_invalid, Input}
    end,
    
    {reply, Result, State};

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