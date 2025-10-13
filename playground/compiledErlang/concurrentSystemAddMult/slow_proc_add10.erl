-module(slow_proc_add10).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

-export([start_link/0, start_link/1, process/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, slow_proc_add10}, ?MODULE, [undefined], []).

start_link(Mul2Pid) ->
    gen_server:start_link({local, slow_proc_add10}, ?MODULE, [Mul2Pid], []).

init([Mul2Pid]) ->
    {ok, Mul2Pid}.

%%% API

process(Pid, Number) ->
    gen_server:call(Pid, {process, Number}, 10000).  % Longer timeout

%%% gen_server callbacks

handle_call({process, Number}, From, Mul2Pid) ->
    %% Spawn a process to handle the computation concurrently
    spawn(fun() ->
        WorkerPid = self(),
        StartTime = erlang:system_time(millisecond),
        io:format("[~p ms] SLOW_ADD10 Worker ~p: Starting to process ~p~n", 
                 [StartTime, WorkerPid, Number]),
        
        %% Simulate some work with a delay
        timer:sleep(500),  % 500ms delay to make concurrency visible
        
        %% Add 10 to Number
        Result1 = Number + 10,
        AfterAddTime = erlang:system_time(millisecond),
        io:format("[~p ms] SLOW_ADD10 Worker ~p: Added 10 to ~p, result: ~p~n", 
                 [AfterAddTime, WorkerPid, Number, Result1]),
        
        %% Forward to Mul2Pid for multiply by 2
        FinalResult = case Mul2Pid of
            undefined -> 
                io:format("[~p ms] SLOW_ADD10 Worker ~p: No mul2 process, returning ~p~n", 
                         [erlang:system_time(millisecond), WorkerPid, Result1]),
                {ok, Result1};
            _ -> 
                try
                    io:format("[~p ms] SLOW_ADD10 Worker ~p: Forwarding ~p to proc_mul2~n", 
                             [erlang:system_time(millisecond), WorkerPid, Result1]),
                    slow_proc_mul2:process(Mul2Pid, Result1)
                catch
                    Class:Reason:Stacktrace ->
                        io:format("Error calling proc_mul2: ~p:~p~n~p~n", 
                                [Class, Reason, Stacktrace]),
                        {error, mul2_failed}
                end
        end,
        
        EndTime = erlang:system_time(millisecond),
        TotalTime = EndTime - StartTime,
        io:format("[~p ms] SLOW_ADD10 Worker ~p: COMPLETE - replying with ~p (total time: ~p ms)~n", 
                 [EndTime, WorkerPid, FinalResult, TotalTime]),
        gen_server:reply(From, FinalResult)
    end),
    {noreply, Mul2Pid};

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


