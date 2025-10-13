-module(simple_wrapper).
-behaviour(gen_server).
-export([start_link/0, call/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

%% Public API - much simpler now with context injection!
-spec call(pid() | atom(), term(), reference() | undefined, atom()) -> any().
call(To, Msg, undefined, Module) ->
    Context = make_ref(),
    call(To, Msg, Context, Module);
call(To, Msg, Context, Module) ->
    io:format("[wrapper] Calling ~p with message ~p, context ~p, from module ~p~n", 
             [To, Msg, Context, Module]),
    
    %% Send monitor notification
    ToModule = get_target_module(To),
    monitor ! {Module, ToModule, Msg, Context},
    
    %% Send single atomic message with context embedded
    Reply = gen_server:call(To, {with_context, Context, Msg}),
    
    io:format("[wrapper] received ~p from ~p with context ~p (originated from module ~p)~n", 
             [Reply, To, Context, Module]),
    
    %% Send response monitor notification
    monitor ! {ToModule, Module, Reply, Context},
    
    Reply.

%% Helper function to determine target module name
get_target_module(To) when is_atom(To) -> 
    To;
get_target_module(To) when is_pid(To) ->
    case process_info(To, registered_name) of
        {registered_name, Name} -> Name;
        [] -> To;
        undefined -> To
    end.

%% No longer need complex queue management!
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.