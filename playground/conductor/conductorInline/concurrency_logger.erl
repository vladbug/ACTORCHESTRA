-module(concurrency_logger).
-export([start/0, stop/0, log/2, log/3]).

-define(LOG_FILE, "concurrency_log.txt").

start() ->
    case whereis(concurrency_logger) of
        undefined ->
            init_log_file(),
            Pid = spawn(fun() -> log_loop() end),
            register(concurrency_logger, Pid),
            log(system, "Concurrency logger started"),
            {ok, Pid};
        Pid ->
            {ok, Pid}
    end.

stop() ->
    case whereis(concurrency_logger) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop,
            unregister(concurrency_logger),
            ok
    end.

%% Public API for logging
log(Module, Message) ->
    log(Module, undefined, Message).

log(Module, Context, Message) ->
    case whereis(concurrency_logger) of
        undefined -> 
            ok; % Logger not started, skip
        Pid ->
            Pid ! {log, Module, Context, Message, erlang:system_time(millisecond), self()}
    end.

%% Initialize log file
init_log_file() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
    Header = io_lib:format("=== CONCURRENCY LOG Started at ~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w ===~n", 
                          [Year, Month, Day, Hour, Min, Sec]),
    file:write_file(?LOG_FILE, Header, [write]).

%% Main logging loop
log_loop() ->
    receive
        stop ->
            file:write_file(?LOG_FILE, "\n=== CONCURRENCY LOG STOPPED ===\n", [append]),
            ok;
        {log, Module, Context, Message, Timestamp, Pid} ->
            ContextStr = case Context of
                undefined -> "no_ctx";
                _ -> io_lib:format("~p", [Context])
            end,
            LogEntry = io_lib:format("[~p ms] [~p:~p] ~s | ~p~n", 
                                   [Timestamp, Module, Pid, Message, ContextStr]),
            file:write_file(?LOG_FILE, LogEntry, [append]),
            log_loop();
        _ ->
            log_loop()
    end.