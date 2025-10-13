-module(slow_proc_add10).
-behaviour(gen_server).
-compile({parse_transform, context_injector}).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, slow_proc_add10}, ?MODULE, [undefined], []).

start_link(Mul2Pid) ->
    gen_server:start_link({local, slow_proc_add10}, ?MODULE, [Mul2Pid], []).

init([Mul2Pid]) ->
    {ok, #{mul2_pid => Mul2Pid}}.

handle_call({process, Number}, From, State) ->
    Mul2Pid = maps:get(mul2_pid, State),
    
    spawn(fun() ->
        Result1 = Number + 10,
        FinalResult = case Mul2Pid of
            undefined -> 
                {ok, Result1};
            _ -> 
                try
                    gen_server:call(Mul2Pid, {process, Result1})
                catch
                    Class:Reason:Stacktrace ->
                        {error, mul2_failed}
                end
        end,
        
        gen_server:reply(From, FinalResult)
    end),
    
    {noreply, State};

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