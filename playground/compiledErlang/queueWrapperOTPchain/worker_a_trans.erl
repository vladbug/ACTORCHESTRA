%% worker_a.erl - AFTER applying parse transform
%% (This is what the compiler sees after transformation)
-module(worker_a_trans).
-behaviour(gen_server).

%% The parse transform injected this record definition
-record(state, {
    context = undefined
}).

%% API
-export([start_link/0, process_data/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process_data(Data, Options) ->
    gen_server:call(?MODULE, {process, {Data, Options}}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

%% The parse transform injected this context handler clause at the top
handle_call({context, Context}, _From, State) ->
    {reply, ok, State#state{context = Context}};

handle_call({process, {Data, Options}}, _From, State) ->
    %% These gen_server:call lines were automatically transformed:
    %% FROM: gen_server:call(worker_b, {transform, Data})
    %% TO:   wrapper:call(worker_b, {transform, Data}, State#state.context)
    Result1 = wrapper:call(worker_b, {transform, Data}, State#state.context),
    
    %% FROM: gen_server:call(worker_c, {validate, Result1, Options})
    %% TO:   wrapper:call(worker_c, {validate, Result1, Options}, State#state.context)
    Result2 = wrapper:call(worker_c, {validate, Result1, Options}, State#state.context),
    
    %% Normal processing
    FinalResult = process_results(Result1, Result2),
    
    {reply, FinalResult, State};

handle_call(Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

process_results(R1, R2) ->
    {processed, R1, R2}.