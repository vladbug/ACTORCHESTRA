-module(add_server).
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
    %% Set context in state
    StateWithContext = State#state{current_context = Context},
    io:format("[~p] Context ~p injected, delegating message ~p~n", [?MODULE, Context, Msg]),
    
    %% Call the original handler exactly as if wrapper never existed
    ?MODULE:handle_call(Msg, From, StateWithContext);

%% Original handlers
handle_call({add_ten, Number}, _From, State) ->
    %% Extract context from state for wrapper calls
    Context = State#state.current_context,
    
    io:format("[add_server] Adding 10 to ~p (context: ~p)~n", [Number, Context]),
    
    %% Simulate some processing time
    timer:sleep(100),
    
    Result = Number + 10,
    io:format("[add_server] ~p + 10 = ~p~n", [Number, Result]),
    
    %% Forward to mul_server using wrapper with context
    io:format("[add_server] Forwarding ~p to mul_server~n", [Result]),
    FinalResult = simple_wrapper:call(mul_server, {multiply_two, Result}, Context, add_server),
    
    io:format("[add_server] Final result from chain: ~p~n", [FinalResult]),
    {reply, FinalResult, State};

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