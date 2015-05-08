-module(web_utils).
-export([
    create_query_url/2,
    create_query_url/3,
    get_user_id/2,
    is_blocked/2,
    querystring/1,
    querystring/2
]).

binary_join([First|Binaries], Separator) ->
    lists:foldl(fun(Before, Current) ->
        <<Before/binary, Separator/binary ,Current/binary>>
    end, First, Binaries).

querystring({Key,Param},JSONEncode) when is_list(Key) ->
    Key2 = erlang:list_to_binary(Key),
    querystring({Key2,Param},JSONEncode);
querystring({Key,Param},JSONEncode) when is_atom(Key) ->
    Key2 = erlang:atom_to_binary(Key,utf8),
    querystring({Key2,Param},JSONEncode);
querystring({Key,Param},JSONEncode) when is_binary(Key) ->
    Value = if
        JSONEncode ->
            jiffy:encode(Param);
        true ->
            Param
    end,
    Separator = <<"=">>,
    <<Key/binary, Separator/binary, Value/binary>>;
querystring([],_JSONEncode) ->
    <<>>;
querystring(Params,JSONEncode) when is_list(Params) ->
    Binaries = lists:map(fun (P) -> querystring(P,JSONEncode) end, Params),
    QS = binary_join(Binaries, <<"&">>),
    Question = <<"?">>,
    <<Question/binary, QS/binary>>.
querystring(Params) when is_list(Params) ->
    querystring(Params,true).

get_user_id(Req,State) ->
    case cowboy_req:binding(userid, Req) of
        <<"me">> ->
            proplists:get_value(user, State);
        Id ->
            Id
    end.

%Determines if a user is allowed to add a list of users to a thread
is_blocked(Uid,UsersToAdd) ->
    {ok,Settings} = application:get_env(homebase,user_api),
    BasePath = proplists:get_value(base_path,Settings),
    URL = BasePath ++ "/users/" ++ binary_to_list(Uid),
    {ok,{_,	_,UserBody}} = httpc:request(URL),
    {UserData} = jiffy:decode(UserBody),
    UserBlockedBy = sets:from_list(proplists:get_value(<<"blockedBy">>,UserData, [])),
    sets:intersection(UserBlockedBy,UsersToAdd) =/= sets:new().

create_query_url(QueryBase,Params) ->
    create_query_url(QueryBase,Params,true).
create_query_url(QueryBase,Params,JSONEncode) ->
    QueryString = querystring(Params,JSONEncode),
    <<QueryBase/binary, QueryString/binary>>.
