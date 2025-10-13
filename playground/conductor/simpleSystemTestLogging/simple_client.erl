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
    ReceiveTime = erlang:system_time(millisecond),
    io:format("[~p ms] [CLIENT] RECEIVED with_context: ctx=~p, msg=~p~n", [ReceiveTime, Context, Msg]),
    
    %% Set context in state
    StateWithContext = State#state{current_context = Context},
    io:format("[~p ms] [CLIENT] Context ~p injected, delegating message ~p~n", [ReceiveTime, Context, Msg]),
    
    %% Call the original handler exactly as if wrapper never existed
    ?MODULE:handle_call(Msg, From, StateWithContext);

%% Original handlers
handle_call({send_number, Number}, _From, State) ->
    StartTime = erlang:system_time(millisecond),
    %% Extract context from state for wrapper calls
    Context = State#state.current_context,
    ClientName = State#state.name,
    
    io:format("[~p ms] [CLIENT-~p] START processing number ~p with context ~p~n", 
             [StartTime, ClientName, Number, Context]),
    
    %% Use wrapper to call add_server
    WrapperCallStart = erlang:system_time(millisecond),
    io:format("[~p ms] [CLIENT-~p] CALLING wrapper for add_server...~n", [WrapperCallStart, ClientName]),
    
    Result = simple_wrapper:call(add_server, {add_ten, Number}, Context, simple_client),
    
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    io:format("[~p ms] [CLIENT-~p] COMPLETE: Final result: ~p (total duration: ~p ms)~n", 
             [EndTime, ClientName, Result, Duration]),
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