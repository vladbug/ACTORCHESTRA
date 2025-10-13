-module(simple_wrapper).
-behaviour(gen_server).
-export([start_link/0, call/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

%% Public API - same as before
-spec call(pid() | atom(), term(), reference() | undefined, atom()) -> any().
call(To, Msg, undefined, Module) ->
    Context = make_ref(),
    call(To, Msg, Context, Module);
call(To, Msg, Context, Module) ->
    StartTime = erlang:system_time(millisecond),
    io:format("[~p ms] [WRAPPER] START: ~p->~p, msg=~p, ctx=~p~n", 
             [StartTime, Module, To, Msg, Context]),
    
    %% This becomes a gen_server:call to enable async processing
    gen_server:call(?MODULE, {process_call, To, Msg, Context, Module}).

%% NEW: Handle the call by spawning async worker
handle_call({process_call, To, Msg, Context, Module}, From, State) ->
    StartTime = erlang:system_time(millisecond),
    io:format("[~p ms] [WRAPPER_SERVER] Received call, spawning worker...~n", [StartTime]),
    
    %% Send monitor notification before spawning
    ToModule = get_target_module(To),
    monitor ! {Module, ToModule, Msg, Context},
    
    %% Spawn worker process to do the actual work
    spawn(fun() ->
        worker_process(To, Msg, Context, Module, From, ToModule)
    end),
    
    %% Return immediately - worker will send reply later
    {noreply, State}.

%% Worker process does the actual communication
worker_process(To, Msg, Context, Module, From, ToModule) ->
    WorkerStartTime = erlang:system_time(millisecond),
    io:format("[~p ms] [WRAPPER_WORKER] Starting work...~n", [WorkerStartTime]),
    
    try
        %% Make atomic call with context
        CallStartTime = erlang:system_time(millisecond),
        io:format("[~p ms] [WRAPPER_WORKER] CALLING: gen_server:call(~p, {with_context, ~p, ~p})~n", 
                 [CallStartTime, To, Context, Msg]),
        
        Reply = gen_server:call(To, {with_context, Context, Msg}),
        
        CallEndTime = erlang:system_time(millisecond),
        io:format("[~p ms] [WRAPPER_WORKER] RECEIVED: reply=~p~n", [CallEndTime, Reply]),
        
        %% Back-propagate context to calling process
        
        {CallingPid, _Ref} = From,
      
        gen_server:cast(CallingPid, {context, Context}),
        io:format("[~p ms] [WRAPPER_WORKER] Context ~p sent to ~p via cast~n", 
                [CallEndTime, Context, CallingPid]),
        
        EndTime = erlang:system_time(millisecond),
        Duration = EndTime - WorkerStartTime,
        io:format("[~p ms] [WRAPPER_WORKER] COMPLETE: Sending reply ~p (took ~p ms)~n", 
                 [EndTime, Reply, Duration]),
        
        %% Send monitor notification for response
        monitor ! {ToModule, Module, Reply, Context},
        
        %% Send reply back to original caller
        gen_server:reply(From, Reply)
        
    catch
        Error:Reason:Stacktrace ->
            io:format("[WRAPPER_WORKER] ERROR: ~p:~p~n~p~n", [Error, Reason, Stacktrace]),
            gen_server:reply(From, {error, processing_failed})
    end.

%% Helper function to determine target module name
get_target_module(To) when is_atom(To) -> 
    To;
get_target_module(To) when is_pid(To) ->
    case process_info(To, registered_name) of
        {registered_name, Name} -> Name;
        [] -> To;
        undefined -> To
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.