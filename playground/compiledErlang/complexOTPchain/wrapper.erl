-module(wrapper).
-behaviour(gen_server).
-export([start_link/0, call/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(operation, {
    from :: {pid(), reference()},
    msg :: term(),
    context :: reference()
}).

-record(state, {
    %% Map of target process -> {queue of operations, currently processing?}
    process_queues = #{} :: #{pid() | atom() => {queue:queue(#operation{}), boolean()}}
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

%% Public API - UNCHANGED from your original
-spec call(pid() | atom(), term(), reference() | undefined) -> any().
call(To, Msg, undefined) ->
    Context = make_ref(),
    gen_server:call(?MODULE, {send_with_context, To, Msg, Context});
call(To, Msg, Context) ->
    io:format("Calling ~p with message ~p and context ~p~n", [To, Msg, Context]),
    gen_server:call(?MODULE, {send_with_context, To, Msg, Context}).

handle_call({send_with_context, To, Msg, Context}, From, State) ->
    Operation = #operation{from = From, msg = Msg, context = Context},
    
    {Queue, IsProcessing} = maps:get(To, State#state.process_queues, {queue:new(), false}),
    NewQueue = queue:in(Operation, Queue),
    
    case IsProcessing of
        false ->
            %% Start processing this queue
            NewState = State#state{
                process_queues = maps:put(To, {NewQueue, true}, State#state.process_queues)
            },
            spawn_link(fun() -> process_next_operation(To, NewQueue) end),
            {noreply, NewState};
        true ->
            %% Another operation is in progress, just queue this one
            NewState = State#state{
                process_queues = maps:put(To, {NewQueue, true}, State#state.process_queues)
            },
            {noreply, NewState}
    end;

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

%% Called by spawned process when an operation completes
handle_cast({operation_complete, To}, State) ->
    case maps:get(To, State#state.process_queues, undefined) of
        undefined ->
            {noreply, State};
        {Queue, true} ->
            case queue:out(Queue) of
                {empty, _} ->
                    %% Queue is empty, stop processing
                    NewState = State#state{
                        process_queues = maps:remove(To, State#state.process_queues)
                    },
                    {noreply, NewState};
                {{value, _JustProcessed}, RemainingQueue} ->
                    case queue:is_empty(RemainingQueue) of
                        true ->
                            %% No more operations, stop processing
                            NewState = State#state{
                                process_queues = maps:remove(To, State#state.process_queues)
                            },
                            {noreply, NewState};
                        false ->
                            %% More operations to process
                            NewState = State#state{
                                process_queues = maps:put(To, {RemainingQueue, true}, State#state.process_queues)
                            },
                            spawn_link(fun() -> process_next_operation(To, RemainingQueue) end),
                            {noreply, NewState}
                    end
            end;
        {_, false} ->
            %% This shouldn't happen, but handle gracefully
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Process the next operation in the queue for a target process
%% This ensures serialized execution per target
process_next_operation(To, Queue) ->
    case queue:out(Queue) of
        {empty, _} ->
            gen_server:cast(?MODULE, {operation_complete, To});
        {{value, #operation{from = From, msg = Msg, context = Context}}, _} ->
            try
                io:format("[wrapper] sending ~p to ~p with context ~p~n", [Msg, To, Context]),
                %% CRITICAL: These two calls are now serialized per target process
                ok = gen_server:call(To, {context, Context}),
                Reply = gen_server:call(To, Msg),
                io:format("[wrapper] received ~p from ~p with context ~p~n", [Reply, To, Context]),
                gen_server:reply(From, Reply)
            catch
                Error:Reason ->
                    io:format("[wrapper] Error processing ~p: ~p:~p~n", [To, Error, Reason]),
                    gen_server:reply(From, {error, {Error, Reason}})
            end,
            gen_server:cast(?MODULE, {operation_complete, To})
    end.