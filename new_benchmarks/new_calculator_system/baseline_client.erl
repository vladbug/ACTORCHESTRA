%% baseline_client.erl - Client without instrumentation
-module(baseline_client).
-behaviour(gen_server).

%% Only gen_server functions exported, no API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% gen_server startup only

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) ->
    {ok, #{}}.

handle_call(connect, _From, State) ->
    %% Direct call without any instrumentation
    Reply = gen_server:call(baseline_slow_central_server, {connect, self()}),
    {reply, Reply, State};

handle_call({send_number, Number}, _From, State) ->
    %% Direct call without any instrumentation
    Result = gen_server:call(baseline_slow_central_server, {process, self(), Number}),
    {reply, Result, State};

handle_call(disconnect, _From, State) ->
    %% Direct call without any instrumentation
    Reply = gen_server:call(baseline_slow_central_server, {disconnect, self()}),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.