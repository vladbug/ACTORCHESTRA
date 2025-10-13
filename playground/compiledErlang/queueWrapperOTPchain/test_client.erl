-module(test_client).
-export([start_client/2]).

%% Start a client process that makes multiple requests
start_client(ClientId, NumRequests) ->
    spawn(fun() -> client_loop(ClientId, NumRequests, 1, []) end).

%% Main client loop
client_loop(ClientId, MaxRequests, RequestNum, Results) when RequestNum > MaxRequests ->
    %% All requests completed, report results
    io:format("[client ~p] Completed all requests. Results: ~p~n", [ClientId, Results]),
    
    %% Verify that all contexts match expectations
    ContextErrors = check_context_integrity(ClientId, Results),
    case ContextErrors of
        [] ->
            io:format("[client ~p] All contexts correct!~n", [ClientId]);
        Errors ->
            io:format("[client ~p] Context errors: ~p~n", [ClientId, Errors])
    end;

client_loop(ClientId, MaxRequests, RequestNum, Results) ->
    %% Create unique context and data for this request
    Context = make_ref(),
    Data = {client_data, ClientId, RequestNum},
    
    io:format("[client ~p] Sending request #~p with context ~p~n", [ClientId, RequestNum, Context]),
    
    %% Make the call through wrapper
    Result = wrapper:call(test_worker, {process, Data}, Context),
    
    io:format("[client ~p] Received reply for request #~p: ~p~n", [ClientId, RequestNum, Result]),
    
    %% Store result with expected context for verification
    ResultWithExpected = {Result, Context},
    
    %% Small random delay to increase concurrency
    timer:sleep(rand:uniform(50)),
    
    client_loop(ClientId, MaxRequests, RequestNum + 1, [ResultWithExpected | Results]).

%% Verify that each result has the correct context
check_context_integrity(ClientId, Results) ->
    lists:foldl(fun({{processed, {client_data, CId, ReqNum}, ActualContext, _WorkerReqNum}, ExpectedContext}, Errors) ->
        case {CId, ActualContext} of
            {ClientId, ExpectedContext} ->
                Errors;  % Correct
            {ClientId, _WrongContext} ->
                [{request, ReqNum, expected, ExpectedContext, got, ActualContext} | Errors];
            {WrongClientId, _} ->
                [{request, ReqNum, wrong_client_id, WrongClientId} | Errors]
        end;
    (Other, Errors) ->
        [{malformed_result, Other} | Errors]
    end, [], Results).