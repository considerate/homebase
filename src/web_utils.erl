-module(web_utils).
-export([
    respond_forbidden/2,
    respond_resource_not_found/1,
    respond_success/3,
    respond_created/3,
    create_query_url/2,
    get_user_id/2
]).

respond_forbidden(Req,Error) ->
    io:format("ERROR: ~p", [Error]),
    cowboy_req:reply(401,Req).

respond_resource_not_found(Req) ->
    cowboy_req:reply(404,Req).

get_user_id(Req,Opts) ->
    case cowboy_req:binding(userid, Req) of
        <<"me">> ->
            Uid = proplists:get_value(user, Opts),
            Uid;
        Id ->
            Id
    end.

respond_success(Req,JSONData,Opts) ->
    BodyText = jiffy:encode(JSONData),
    ResponseHeaders = [{<<"Content-Type">>,<<"application/json">>}],
    Response = cowboy_req:reply(200,
                                ResponseHeaders,
                                BodyText,
                                Req
                               ),
    {ok, Response, Opts}.

respond_created(Req,JSONData,Opts) ->
    BodyText = jiffy:encode(JSONData),
    ResponseHeaders = [{<<"Content-Type">>,<<"application/json">>}],
    Response = cowboy_req:reply(201,
                                ResponseHeaders,
                                BodyText,
                                Req
                               ),
    {ok, Response, Opts}.

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
