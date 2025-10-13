-module(slow_proc_add10).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

%% Only startup functions exported, no API
-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% Startup only

start_link() ->
    gen_server:start_link({local, slow_proc_add10}, ?MODULE, [undefined], []).

start_link(Mul2Pid) ->
    gen_server:start_link({local, slow_proc_add10}, ?MODULE, [Mul2Pid], []).

init([Mul2Pid]) ->
    %% Initialize state as map to work with context injector
    {ok, #{mul2_pid => Mul2Pid}}.

%%% gen_server callbacks

handle_call({process, Number}, From, State) ->
    Mul2Pid = maps:get(mul2_pid, State),
    %% Context extracted by parse_transform at clause header
    
    StartTime = erlang:system_time(millisecond),
    concurrency_logger:log(slow_proc_add10, Context, 
                         io_lib:format("RECV process request ~p", [Number])),
    
    %% ASYNC: Spawn process to handle the request  
    spawn(fun() ->
        WorkerPid = self(),
        concurrency_logger:log(slow_proc_add10, Context, 
                             io_lib:format("WORKER ~p START processing ~p", [WorkerPid, Number])),
        
        Result1 = Number + 10,
        AfterAddTime = erlang:system_time(millisecond),
        concurrency_logger:log(slow_proc_add10, Context, 
                             io_lib:format("WORKER ~p CALC: ~p + 10 = ~p", [WorkerPid, Number, Result1])),
        
        FinalResult = case Mul2Pid of
            undefined -> 
                concurrency_logger:log(slow_proc_add10, Context, 
                                     io_lib:format("WORKER ~p NO MUL2, returning ~p", [WorkerPid, Result1])),
                {ok, Result1};
            _ -> 
                try
                    concurrency_logger:log(slow_proc_add10, Context, 
                                         io_lib:format("WORKER ~p CALLING mul2 with ~p", [WorkerPid, Result1])),
                    %% This gen_server:call will be transformed to:
                    %% simple_wrapper:call(Mul2Pid, {process, Result1}, Context, slow_proc_add10)
                    gen_server:call(Mul2Pid, {process, Result1})
                catch
                    Class:Reason:Stacktrace ->
                        concurrency_logger:log(slow_proc_add10, Context, 
                                             io_lib:format("WORKER ~p ERROR: ~p:~p", [WorkerPid, Class, Reason])),
                        {error, mul2_failed}
                end
        end,
        
        EndTime = erlang:system_time(millisecond),
        TotalTime = EndTime - StartTime,
        concurrency_logger:log(slow_proc_add10, Context, 
                             io_lib:format("WORKER ~p COMPLETE got ~p (took ~p ms)", [WorkerPid, FinalResult, TotalTime])),
        
        gen_server:reply(From, FinalResult)
    end),
    concurrency_logger:log(slow_proc_add10, Context, "SPAWNED async worker, ready for next request"),
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