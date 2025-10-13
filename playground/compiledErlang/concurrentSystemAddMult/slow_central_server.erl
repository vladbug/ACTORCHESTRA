-module(slow_central_server).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

%% API
-export([start_link/0, connect_client/1, disconnect_client/1, process_request/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    add10,      %% pid of slow_proc_add10
    mul2,       %% pid of slow_proc_mul2
    clients = [] %% list of connected clients (pids)
}).

%%% API Functions

start_link() ->
    gen_server:start_link({local, slow_central_server}, ?MODULE, [], []).

connect_client(ClientPid) ->
    gen_server:call(slow_central_server, {connect, ClientPid}).

disconnect_client(ClientPid) ->
    gen_server:call(slow_central_server, {disconnect, ClientPid}).

process_request(ClientPid, Number) ->
    gen_server:call(slow_central_server, {process, ClientPid, Number}, 15000).

%%% gen_server callbacks

init([]) ->
    %% Start the slow multiply process first
    {ok, Mul2Pid} = slow_proc_mul2:start_link(),
    %% Then start slow add10 with Mul2Pid
    {ok, Add10Pid} = slow_proc_add10:start_link(Mul2Pid),
    {ok, #state{add10 = Add10Pid, mul2 = Mul2Pid}}.

handle_call({connect, ClientPid}, _From, State) ->
    %% Add client if not already connected
    Clients = State#state.clients,
    NewClients = lists:usort([ClientPid | Clients]),
    io:format("SLOW_CENTRAL: Client connected: ~p~n", [ClientPid]),
    {reply, ok, State#state{clients = NewClients}};

handle_call({disconnect, ClientPid}, _From, State) ->
    Clients = State#state.clients,
    NewClients = lists:delete(ClientPid, Clients),
    io:format("SLOW_CENTRAL: Client disconnected: ~p~n", [ClientPid]),
    {reply, ok, State#state{clients = NewClients}};

handle_call({process, ClientPid, Number}, From, State) ->
    Clients = State#state.clients,
    case lists:member(ClientPid, Clients) of
        true ->
            %% Spawn a process to handle the request concurrently
            spawn(fun() ->
                WorkerPid = self(),
                Add10Pid = State#state.add10,
                StartTime = erlang:system_time(millisecond),
                io:format("[~p ms] SLOW_CENTRAL Worker ~p: Processing request ~p from client ~p~n", 
                         [StartTime, WorkerPid, Number, ClientPid]),
                
                %% Forward request to add10 process and wait for result
                try
                    Result = slow_proc_add10:process(Add10Pid, Number),
                    EndTime = erlang:system_time(millisecond),
                    TotalTime = EndTime - StartTime,
                    io:format("[~p ms] SLOW_CENTRAL Worker ~p: COMPLETE - replying to client ~p with ~p (total: ~p ms)~n", 
                             [EndTime, WorkerPid, ClientPid, Result, TotalTime]),
                    gen_server:reply(From, Result)
                catch
                    Class:Reason:Stacktrace ->
                        io:format("Error in slow_central_server processing: ~p:~p~n~p~n", 
                                [Class, Reason, Stacktrace]),
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
