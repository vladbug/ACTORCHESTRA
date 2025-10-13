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

-spec call(pid() | atom(), term(), reference() | undefined, atom()) -> any().
call(To, Msg, undefined, Module) ->
    %% No context provided - create a new one
    Context = make_ref(),
    call(To, Msg, Context, Module);
call(To, Msg, Context, Module) ->
    %% Forward the call with the provided context
    gen_server:call(?MODULE, {process_call, To, Msg, Context, Module}).

%% Check if calling module is a client that should maintain context
is_client_module(client) -> true;
is_client_module(_) -> false.

%% Handle the call by spawning async worker
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
worker_process(To, Msg, Context, Module, From, ToModule) ->
    try
        Reply = gen_server:call(To, {with_context, Context, Msg}),
       
        %% Return different formats based on calling module type
        FinalReply = case is_client_module(Module) of
            true ->
                %% Client modules get {Reply, Context} tuple
                {Reply, Context};
            false ->
                %% Non-client modules get just Reply
                Reply
        end,
       
        %% Send monitor notification for response
        monitor ! {ToModule, Module, Reply, Context},
       
        %% Send reply back to original caller
        gen_server:reply(From, FinalReply)
    catch
        _Error:_Reason:_Stacktrace ->
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