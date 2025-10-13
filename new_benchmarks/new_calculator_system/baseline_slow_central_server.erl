-module(baseline_slow_central_server).
-behaviour(gen_server).

%% Only startup function exported, no API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    add10,      %% pid of baseline_slow_proc_add10
    mul2,       %% pid of baseline_slow_proc_mul2
    clients = [] %% list of connected clients (pids)
}).

%%% Startup only

start_link() ->
    gen_server:start_link({local, baseline_slow_central_server}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) ->
    %% Start the slow multiply process first
    {ok, Mul2Pid} = baseline_slow_proc_mul2:start_link(),
    %% Then start slow add10 with Mul2Pid
    {ok, Add10Pid} = baseline_slow_proc_add10:start_link(Mul2Pid),
    {ok, #state{add10 = Add10Pid, mul2 = Mul2Pid}}.

handle_call({connect, ClientPid}, _From, State) ->
    Clients = State#state.clients,
    NewClients = lists:usort([ClientPid | Clients]),
    {reply, ok, State#state{clients = NewClients}};

handle_call({disconnect, ClientPid}, _From, State) ->
    Clients = State#state.clients,
    NewClients = lists:delete(ClientPid, Clients),
    {reply, ok, State#state{clients = NewClients}};

handle_call({process, ClientPid, Number}, From, State) ->
    Clients = State#state.clients,
    case lists:member(ClientPid, Clients) of
        true ->
            Add10Pid = State#state.add10,
            
            %% ASYNC: Spawn process to handle the request (no instrumentation)
            spawn(fun() ->
                try
                    %% Direct gen_server call without wrapper
                    Result = gen_server:call(Add10Pid, {process, Number}),
                    gen_server:reply(From, Result)
                catch
                    Class:Reason:Stacktrace ->
                        gen_server:reply(From, {error, processing_failed})
                end
            end),
            {noreply, State};
        false ->
            {reply, {error, not_connected}, State}
    end;

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