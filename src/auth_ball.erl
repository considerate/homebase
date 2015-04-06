-module(auth_ball).
-export([authenticate/1, rest_auth/2, secret/0]).
-define (SECRET, <<"This is a very secret secret, do not tell anyone.">>).

secret() ->
    ?SECRET.

authenticate(Req) ->
    Headers = cowboy_req:headers(Req),
    {_,Auth}=proplists:lookup(<<"authorization">>,Headers),
    <<"Bearer ", Token/binary>> = Auth,
    case ejwt:parse_jwt(Token, ?SECRET) of
        {Data} -> {ok, Data};
        Error -> {error, Error}
    end.

rest_auth(Req, State) ->
    case authenticate(Req) of
        {ok, Data} ->
            {true, Req, [{user,proplists:get_value(<<"id">>,Data)}|State]};
        _ ->
            {{false, <<"Authorization">>}, Req, State}
    end.

