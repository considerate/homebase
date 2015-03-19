-module(auth_ball).
-export([authenticate/1, secret/0]).
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
