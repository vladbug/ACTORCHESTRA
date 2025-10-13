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

%% Public API - extract context from calling process if available
-spec call(pid() | atom(), term(), reference() | undefined, atom()) -> any().
call(To, Msg, undefined, Module) ->
    %% Try to get existing context from calling process
    Context = case is_client_module(Module) of
        true ->
            case get_client_context() of
                undefined -> make_ref();  % Generate new context
                ExistingContext -> ExistingContext  % Reuse existing
            end;
        false ->
            make_ref()  % Non-client modules always get new context
    end,
    call(To, Msg, Context, Module);
call(To, Msg, Context, Module) ->
    %% This becomes a gen_server:call to enable async processing
    gen_server:call(?MODULE, {process_call, To, Msg, Context, Module}).

%% Check if calling module is a client that should maintain context
is_client_module(client) -> true;
is_client_module(_) -> false.

%% Get context from calling process state if it's a client
get_client_context() ->
    try
        % Try to get context from client process
        CallingPid = self(),
        case gen_server:call(CallingPid, get_current_context, 100) of
            {ok, Context} -> Context;
            _ -> undefined
        end
    catch
        _:_ -> undefined
    end.

%% NEW: Handle the call by spawning async worker
handle_call({process_call, To, Msg, Context, Module}, From, State) ->
    %% Send monitor notification before spawning
    ToModule = get_target_module(To),
    monitor ! {Module, ToModule, Msg, Context},
    
    %% Spawn worker process to do the actual work
    spawn(fun() ->
        worker_process(To, Msg, Context, Module, From, ToModule)
    end),
    
    %% Return immediately - worker will send reply later
    {noreply, State}.

%% Worker process does the actual communication
worker_process(To, Msg, Context, Module, From, ToModule) ->
   
    
    try
        Reply = gen_server:call(To, {with_context, Context, Msg}),
        
        %% Return different formats based on calling module type
        FinalReply = case is_client_module(Module) of
            true ->
                
                {Reply, Context};
            false ->
                
                Reply
        end,
        
        %% Send monitor notification for response
        io:format("Sending monitor notification: ~p~n", [{ToModule, Module, Reply, Context}]),
        monitor ! {ToModule, Module, Reply, Context},
   
        
        %% Send reply back to original caller
        gen_server:reply(From, FinalReply)
    catch
        Error:Reason:Stacktrace ->
            gen_server:reply(From, {error, processing_failed})
    end.

%% Helper function to determine target module name
get_target_module(To) when is_atom(To) -> 
    To;
get_target_module(To) when is_pid(To) ->
    case process_info(To, registered_name) of
        {registered_name, Name} -> Name;
        [] -> To;
        undefined -> To
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.