-module(web_utils).
-export([
    create_query_url/2,
    get_user_id/2,
    is_blocked/2,
    querystring/1
]).

binary_join([First|Binaries], Separator) ->
    lists:foldl(fun(Before, Current) ->
        <<Before/binary, Separator/binary ,Current/binary>>
    end, First, Binaries).

querystring({Key,Param}) when is_list(Key) ->
    Key2 = erlang:list_to_binary(Key),
    querystring({Key2,Param});
querystring({Key,Param}) when is_atom(Key) ->
    Key2 = erlang:atom_to_binary(Key,utf8),
    querystring({Key2,Param});
querystring({Key,Param}) when is_binary(Key) ->
    JSON = jiffy:encode(Param),
    Separator = <<"=">>,
    <<Key/binary, Separator/binary, JSON/binary>>;
querystring([]) ->
    <<>>;
querystring(Params) when is_list(Params) ->
    Binaries = lists:map(fun querystring/1, Params),
    QS = binary_join(Binaries, <<"&">>),
    Question = <<"?">>,
    <<Question/binary, QS/binary>>.


get_user_id(Req,Opts) ->
    case cowboy_req:binding(userid, Req) of
        <<"me">> ->
            proplists:get_value(user, Opts);
        Id ->
            Id
    end.

is_blocked(Uid,UsersToAdd) ->
    {ok,Settings} = application:get_env(homebase,user_api),
    BasePath = proplists:get_value(base_path,Settings),
    URL = BasePath ++ "/users/" ++ binary_to_list(Uid),
    {ok,{_,	_,UserBody}} = httpc:request(URL),
    {UserData} = jiffy:decode(UserBody),
    Blocking = proplists:get_value(<<"blocking">>,UserData),
    lists:any(fun(Blocked) -> sets:is_element(Blocked, UsersToAdd) end, Blocking).

create_query_url(QueryBase,Params) ->
    QueryString = querystring(Params),
    <<QueryBase/binary, QueryString/binary>>.
