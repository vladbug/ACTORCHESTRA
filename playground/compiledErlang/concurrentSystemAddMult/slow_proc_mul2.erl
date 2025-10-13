%% slow_proc_mul2.erl - Version with artificial delays for testing concurrency
-module(slow_proc_mul2).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

-export([start_link/0, process/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Called by other processes to process a number
process(ServerPid, Number) ->
    gen_server:call(ServerPid, {process, Number}, 10000).  % Longer timeout

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, undefined}.

handle_call({process, Number}, From, State) ->
    %% Spawn a process to handle the multiplication concurrently
    spawn(fun() ->
        WorkerPid = self(),
        StartTime = erlang:system_time(millisecond),
        io:format("[~p ms] SLOW_MUL2 Worker ~p: Starting to process ~p~n", 
                 [StartTime, WorkerPid, Number]),
        
        %% Simulate some work with a delay
        timer:sleep(300),  % 300ms delay
        
        Result = Number * 2,
        EndTime = erlang:system_time(millisecond),
        TotalTime = EndTime - StartTime,
        
        io:format("[~p ms] SLOW_MUL2 Worker ~p: COMPLETE - ~p * 2 = ~p (took ~p ms)~n", 
                 [EndTime, WorkerPid, Number, Result, TotalTime]),
        gen_server:reply(From, {ok, Result})
    end),
    {noreply, State};

handle_call(_, _From, State) ->
    {reply, {error, bad_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
