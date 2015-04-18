-module(auth_ball).
-export([authenticate/1, rest_auth/2, secret/0, user_in_thread/2]).
-define (SECRET, <<"This is a very secret secret, do not tell anyone.">>).

secret() ->
    ?SECRET.

authenticate(Req) ->
    Headers = cowboy_req:headers(Req),
    case proplists:lookup(<<"authorization">>,Headers) of
        {_, <<"Bearer ", Token/binary>>} -> 
            case ejwt:parse_jwt(Token, ?SECRET) of
                {Data} -> {ok, Data};
                Error -> {error, Error}
            end;
        {_, _} ->
            {error, not_bearer_authorization};
        none ->
            {error, no_authorization_header}
    end.

rest_auth(Req, State) ->
    case authenticate(Req) of
        {ok, Data} ->
            Uid = proplists:get_value(<<"id">>,Data),
            {true, Req, [{user,Uid}|State]};
        {error, Error} ->
            io:format("authentication failed: ~p~n", [Error]),
            {{false, <<"Authorization">>}, Req, State}
    end.

user_in_thread(Uid,{Thread}) when is_list(Thread) andalso is_bitstring(Uid)->
	UsersInThread = proplists:get_value(<<"users">>, Thread),
	lists:member(Uid, UsersInThread);

%When using this overload, make sure that rest_auth has already been called
user_in_thread(State,{Thread}) when is_list(Thread) andalso is_list(State) ->
	Uid = proplists:get_value(user,State),
	user_in_thread(Uid,{Thread}).
