-module(test_worker).
-behaviour(gen_server).
-export([start_link/0, process_request/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    context = undefined,
    request_count = 0
}).

-define(WORKER_DELAY_MS, 100).  % Simulate some processing time

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

process_request(Data) ->
    gen_server:call(?MODULE, {process, Data}).

init([]) ->
    {ok, #state{}}.

%% Context handler - injected by parse transform
handle_call({context, Context}, _From, State) ->
    io:format("[worker] Setting context to ~p~n", [Context]),
    {reply, ok, State#state{context = Context}};

%% Main processing handler
handle_call({process, Data}, _From, State) ->
    %% Simulate some processing time to increase chance of interleaving
    timer:sleep(?WORKER_DELAY_MS),
    
    RequestNum = State#state.request_count + 1,
    Result = {processed, Data, State#state.context, RequestNum},
    
    io:format("[worker] Processed ~p with context ~p (request #~p)~n", 
              [Data, State#state.context, RequestNum]),
    
    NewState = State#state{request_count = RequestNum},
    {reply, Result, NewState};

handle_call(Request, _From, State) ->
    io:format("[worker] Unknown request: ~p~n", [Request]),
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.