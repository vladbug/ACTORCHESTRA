-module(simple_client).
-behaviour(gen_server).

%% API
-export([start_link/1, send_number/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    name,
    current_context = undefined
}).

%%% API

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

send_number(ClientName, Number) ->
    gen_server:call(ClientName, {send_number, Number}).

%%% gen_server callbacks

init([Name]) ->
    {ok, #state{name = Name}}.

%% Special context injection handler (added by context_injector)
handle_call({with_context, Context, Msg}, From, State) ->
    %% Set context in state
    StateWithContext = State#state{current_context = Context},
    io:format("[~p] Context ~p injected, delegating message ~p~n", [?MODULE, Context, Msg]),
    
    %% Call the original handler exactly as if wrapper never existed
    ?MODULE:handle_call(Msg, From, StateWithContext);

%% Original handlers
handle_call({send_number, Number}, _From, State) ->
    %% Extract context from state for wrapper calls
    Context = State#state.current_context,
    ClientName = State#state.name,
    
    io:format("[~p] Processing number ~p with context ~p~n", [ClientName, Number, Context]),
    
    %% Use wrapper to call add_server
    Result = simple_wrapper:call(add_server, {add_ten, Number}, Context, simple_client),
    
    io:format("[~p] Final result: ~p~n", [ClientName, Result]),
    {reply, Result, State};

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