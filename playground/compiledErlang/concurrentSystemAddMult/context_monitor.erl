-module(context_monitor).
-behaviour(gen_server).
-export([start_link/0, trace_context/2, get_trace_log/0, clear_log/0, stop_tracing/0, print_trace_log/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    traces = [],        % List of {Timestamp, Context, ProcessInfo, Message}
    active_contexts = #{} % Map of Context -> {StartTime, OriginalValue}
}).

-record(trace_entry, {
    timestamp,
    context,
    process_name,
    process_pid,
    message_type,
    message_data
}).

%%% API Functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

trace_context(Context, OriginalValue) ->
    gen_server:call(?MODULE, {start_trace, Context, OriginalValue}).

get_trace_log() ->
    gen_server:call(?MODULE, get_trace_log).

clear_log() ->
    gen_server:call(?MODULE, clear_log).

stop_tracing() ->
    gen_server:call(?MODULE, stop_tracing).

%%% gen_server callbacks

init([]) ->
    %% Enable tracing for gen_server calls
    erlang:trace_pattern({gen_server, handle_call, 3}, []),
    erlang:trace(all, true, [call, {tracer, self()}]),
    
    io:format("Context monitor started - tracing gen_server calls~n"),
    {ok, #state{}}.

handle_call({start_trace, Context, OriginalValue}, _From, State) ->
    StartTime = erlang:system_time(millisecond),
    NewActiveContexts = maps:put(Context, {StartTime, OriginalValue}, State#state.active_contexts),
    
    TraceEntry = #trace_entry{
        timestamp = StartTime,
        context = Context,
        process_name = context_monitor,
        process_pid = self(),
        message_type = trace_start,
        message_data = {original_value, OriginalValue}
    },
    
    NewTraces = [TraceEntry | State#state.traces],
    io:format("[MONITOR] Started tracing context ~p with value ~p~n", [Context, OriginalValue]),
    
    {reply, ok, State#state{traces = NewTraces, active_contexts = NewActiveContexts}};

handle_call(get_trace_log, _From, State) ->
    %% Return traces in chronological order (reverse the list)
    SortedTraces = lists:reverse(State#state.traces),
    {reply, SortedTraces, State};

handle_call(clear_log, _From, State) ->
    {reply, ok, State#state{traces = [], active_contexts = #{}}};

handle_call(stop_tracing, _From, State) ->
    erlang:trace(all, false, [call]),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle trace messages
handle_info({trace, Pid, call, {Module, Function, Args}}, State) ->
    %% Look for context-related calls
    case analyze_call(Module, Function, Args) of
        {context_call, Context, MessageType, Data} ->
            case maps:get(Context, State#state.active_contexts, undefined) of
                undefined ->
                    %% Not a context we're tracking
                    {noreply, State};
                _ContextInfo ->
                    %% This is a context we're tracking
                    ProcessName = case erlang:process_info(Pid, registered_name) of
                        {registered_name, Name} -> Name;
                        [] -> unnamed
                    end,
                    
                    TraceEntry = #trace_entry{
                        timestamp = erlang:system_time(millisecond),
                        context = Context,
                        process_name = ProcessName,
                        process_pid = Pid,
                        message_type = MessageType,
                        message_data = Data
                    },
                    
                    NewTraces = [TraceEntry | State#state.traces],
                    io:format("[MONITOR] Context ~p in ~p (~p): ~p~n", 
                             [Context, ProcessName, Pid, MessageType]),
                    
                    {noreply, State#state{traces = NewTraces}}
            end;
        no_context ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    erlang:trace(all, false, [call]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal Functions

analyze_call(gen_server, handle_call, [Request, _From, _State]) ->
    case Request of
        {context, Context} ->
            {context_call, Context, context_propagation, Request};
        {send_number, Number} ->
            {context_call, unknown, send_number, Number};
        {process, Number} ->
            {context_call, unknown, process_request, Number};
        {process, Pid, Number} ->
            {context_call, unknown, process_from_client, {Pid, Number}};
        _ ->
            no_context
    end;
analyze_call(_Module, _Function, _Args) ->
    no_context.

%%% Utility functions for displaying results

print_trace_log() ->
    Traces = get_trace_log(),
    io:format("~n=== CONTEXT TRACE LOG ===~n"),
    case Traces of
        [] ->
            io:format("No traces recorded.~n");
        _ ->
            lists:foreach(fun(#trace_entry{
                timestamp = TS,
                context = Ctx,
                process_name = ProcName,
                process_pid = Pid,
                message_type = MsgType,
                message_data = Data
            }) ->
                io:format("[~p] ~p (~p) in ~p: ~p~n", 
                         [TS, Ctx, MsgType, ProcName, Data])
            end, Traces)
    end,
    io:format("=== END TRACE LOG ===~n").
