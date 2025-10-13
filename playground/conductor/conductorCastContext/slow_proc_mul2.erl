-module(slow_proc_mul2).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

%% Only startup function exported, no API
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Startup only
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Use map-based state to work with context injector
    {ok, #{}}.

handle_call({process, Number}, _From, State) ->
    %% Context extracted by parse_transform at clause header
    
    StartTime = erlang:system_time(millisecond),
    concurrency_logger:log(slow_proc_mul2, Context, 
                         io_lib:format("RECV process request ~p", [Number])),
    
    concurrency_logger:log(slow_proc_mul2, Context, 
                         io_lib:format("CALC: ~p * 2", [Number])),
    
    Result = Number * 2,
    EndTime = erlang:system_time(millisecond),
    TotalTime = EndTime - StartTime,
    
    concurrency_logger:log(slow_proc_mul2, Context, 
                         io_lib:format("COMPLETE: ~p * 2 = ~p (took ~p ms)", [Number, Result, TotalTime])),
    
    {reply, {ok, Result}, State};

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