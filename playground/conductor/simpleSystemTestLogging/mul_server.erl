-module(mul_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    current_context = undefined
}).

%%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

%% Special context injection handler (added by context_injector)
handle_call({with_context, Context, Msg}, From, State) ->
    ReceiveTime = erlang:system_time(millisecond),
    io:format("[~p ms] [MUL_SERVER] RECEIVED with_context: ctx=~p, msg=~p~n", [ReceiveTime, Context, Msg]),
    
    %% Set context in state
    StateWithContext = State#state{current_context = Context},
    io:format("[~p ms] [MUL_SERVER] Context ~p injected, delegating message ~p~n", [ReceiveTime, Context, Msg]),
    
    %% Call the original handler exactly as if wrapper never existed
    ?MODULE:handle_call(Msg, From, StateWithContext);

%% Original handlers - NOW WITH ASYNC PROCESSING!
handle_call({multiply_two, Number}, From, State) ->
    StartTime = erlang:system_time(millisecond),
    %% Extract context from state
    Context = State#state.current_context,
    
    io:format("[~p ms] [MUL_SERVER] ASYNC START: Multiplying ~p by 2 (context: ~p)~n", [StartTime, Number, Context]),
    
    %% SPAWN ASYNC WORKER - Server immediately becomes available for next request!
    spawn(fun() ->
        WorkerStartTime = erlang:system_time(millisecond),
        io:format("[~p ms] [MUL_SERVER_WORKER] Starting async processing for ~p~n", [WorkerStartTime, Number]),
        
        try
            %% Simulate some processing time
            io:format("[~p ms] [MUL_SERVER_WORKER] PROCESSING: Simulating work for ~p ms...~n", [WorkerStartTime, 150]),
            timer:sleep(150),
            
            AfterSleepTime = erlang:system_time(millisecond),
            Result = Number * 2,
            
            EndTime = erlang:system_time(millisecond),
            Duration = EndTime - WorkerStartTime,
            io:format("[~p ms] [MUL_SERVER_WORKER] COMPLETE: ~p * 2 = ~p (worker duration: ~p ms, final result)~n", 
                     [EndTime, Number, Result, Duration]),
            
            %% Send async reply back to original caller
            gen_server:reply(From, {ok, Result}),
            io:format("[~p ms] [MUL_SERVER_WORKER] REPLY SENT to original caller~n", [EndTime])
        catch
            Error:Reason:Stacktrace ->
                io:format("[MUL_SERVER_WORKER] ERROR: ~p:~p~n~p~n", [Error, Reason, Stacktrace]),
                gen_server:reply(From, {error, processing_failed})
        end
    end),
    
    %% CRITICAL: Return immediately with {noreply, State}
    %% This makes the mul_server immediately available for the next request!
    ServerReadyTime = erlang:system_time(millisecond),
    io:format("[~p ms] [MUL_SERVER] ASYNC SPAWNED: Server ready for next request (worker processing in background)~n", 
             [ServerReadyTime]),
    {noreply, State};

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