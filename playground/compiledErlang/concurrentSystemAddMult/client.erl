-compile({parse_transform, context_injector}).
-module(client).
-behaviour(gen_server).

-export([start_link/1, connect/1, send_number/2, disconnect/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% API

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

connect(Name) ->
    gen_server:call(Name, connect).

send_number(Name, Number) ->
    gen_server:call(Name, {send_number, Number}).

disconnect(Name) ->
    gen_server:call(Name, disconnect).

%%% gen_server callbacks

init([]) ->
    {ok, #{}}.

handle_call(connect, _From, State) ->
    %% Send the message the central_server expects: {connect, Pid}
    Reply = gen_server:call(slow_central_server, {connect, self()}),
    {reply, Reply, State};

handle_call({send_number, Number}, _From, State) ->
    %% Send {process, Pid, Number} to central_server
    Result = gen_server:call(slow_central_server, {process, self(), Number}),
    {reply, Result, State};

handle_call(disconnect, _From, State) ->
    Reply = gen_server:call(slow_central_server, {disconnect, self()}),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


