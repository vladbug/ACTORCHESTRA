-module(test_server).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    data = test_data
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

%% Simple handler to test context injection
handle_call(hello, _From, State) ->
    %% Context should be injected here by parse_transform
    io:format("[TEST_SERVER] Hello called with context: ~p~n", [Context]),
    {reply, {ok, hello_response}, State};

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