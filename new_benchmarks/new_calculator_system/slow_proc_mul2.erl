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

handle_call({process, Number}, From, State) ->
    %% Spawn a process to handle the multiplication concurrently
    spawn(fun() ->
        Result = Number * 2,
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