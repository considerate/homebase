-module(web_utils).
-export([
    create_query_url/2,
    get_user_id/2,
    is_blocked/2
]).

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

create_query_url(QueryBase,[{Key0,Param0}|T]) ->
    Query0 = iolist_to_binary([ 
                <<"?">>,
                Key0,
                <<"=">>, 
                Param0]),
    QueryN = lists:foldl(
            fun({KeyK,ParamK},QueryK) ->
                    iolist_to_binary([QueryK, <<"&">>, KeyK, <<"=">>,  ParamK])
            end,
            Query0, T),
    iolist_to_binary([QueryBase, QueryN]).
