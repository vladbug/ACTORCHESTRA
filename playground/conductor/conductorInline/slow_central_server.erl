-module(slow_central_server).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

%% Only startup function exported, no API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    add10,      %% pid of slow_proc_add10
    mul2,       %% pid of slow_proc_mul2
    clients = [] %% list of connected clients (pids)
    %% context field will be automatically added by parse_transform
}).

%%% Startup only

start_link() ->
    gen_server:start_link({local, slow_central_server}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) ->
    %% Start the slow multiply process first
    {ok, Mul2Pid} = slow_proc_mul2:start_link(),
    %% Then start slow add10 with Mul2Pid
    {ok, Add10Pid} = slow_proc_add10:start_link(Mul2Pid),
    {ok, #state{add10 = Add10Pid, mul2 = Mul2Pid}}.

handle_call({connect, ClientPid}, _From, State) ->
    %% Context extracted by parse_transform at clause header
    Clients = State#state.clients,
    NewClients = lists:usort([ClientPid | Clients]),
    concurrency_logger:log(slow_central_server, Context, 
                         io_lib:format("Client connected: ~p", [ClientPid])),
    {reply, ok, State#state{clients = NewClients}};

handle_call({disconnect, ClientPid}, _From, State) ->
    %% Context extracted by parse_transform at clause header
    Clients = State#state.clients,
    NewClients = lists:delete(ClientPid, Clients),
    concurrency_logger:log(slow_central_server, Context, 
                         io_lib:format("Client disconnected: ~p", [ClientPid])),
    {reply, ok, State#state{clients = NewClients}};

handle_call({process, ClientPid, Number}, From, State) ->
    %% Context extracted by parse_transform at clause header
    Clients = State#state.clients,
    case lists:member(ClientPid, Clients) of
        true ->
            Add10Pid = State#state.add10,
            StartTime = erlang:system_time(millisecond),
            concurrency_logger:log(slow_central_server, Context, 
                                 io_lib:format("RECV process request ~p from client ~p", [Number, ClientPid])),
            
            %% ASYNC: Spawn process to handle the request
            spawn(fun() ->
                WorkerPid = self(),
                concurrency_logger:log(slow_central_server, Context, 
                                     io_lib:format("WORKER ~p START processing ~p", [WorkerPid, Number])),
                try
                    %% This gen_server:call will be transformed to:
                    %% simple_wrapper:call(Add10Pid, {process, Number}, Context, slow_central_server)
                    concurrency_logger:log(slow_central_server, Context, 
                                         io_lib:format("WORKER ~p CALLING add10 with ~p", [WorkerPid, Number])),
                    Result = gen_server:call(Add10Pid, {process, Number}),
                    EndTime = erlang:system_time(millisecond),
                    TotalTime = EndTime - StartTime,
                    concurrency_logger:log(slow_central_server, Context, 
                                         io_lib:format("WORKER ~p COMPLETE got ~p (took ~p ms)", [WorkerPid, Result, TotalTime])),
                    gen_server:reply(From, Result)
                catch
                    Class:Reason:Stacktrace ->
                        concurrency_logger:log(slow_central_server, Context, 
                                             io_lib:format("WORKER ~p ERROR: ~p:~p", [WorkerPid, Class, Reason])),
                        gen_server:reply(From, {error, processing_failed})
                end
            end),
            concurrency_logger:log(slow_central_server, Context, "SPAWNED async worker, ready for next request"),
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