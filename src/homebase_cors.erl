-module(homebase_cors).
-behavior(cowboy_middleware).
-export([execute/2]).

execute(Req, Env) ->
    case cowboy_req:header(<<"access-control-request-headers">>, Req) of
        undefined ->
            Req1 = Req;
        Headers ->
            Req1 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, Headers, Req)
    end,
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req2),
    %io:format("Req1 = ~p~n"
    %          "Env  = ~p~n", [Req1, Env]),
    {ok, Req3, Env}.
