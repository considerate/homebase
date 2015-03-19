-module(web_utils).
-export([respond_resource_not_found/2,respond_success/3,create_query_url/2]).

respond_resource_not_found(Req,Error) ->
	io:format("ERROR: ~p", [Error]),
	cowboy_req:reply(401,Req).
	
respond_success(Req,JSONData,Opts) ->
	BodyText = jiffy:encode(JSONData),
	ResponseHeaders = [{<<"Content-Type">>,<<"application/json">>}],
	Response = cowboy_req:reply(200,
		ResponseHeaders,
		BodyText,
		Req
	),
	{ok, Response, Opts}.
	
create_query_url(QueryBase,[{Key0,Param0}|T]) ->
	Query0 = "?" ++ Key0 ++ "=" ++ binary_to_list(jiffy:encode(Param0)),
	QueryN = lists:foldl(
	fun({KeyK,ParamK},QueryK) ->
		QueryK ++ "&" ++ KeyK ++ "=" ++ binary_to_list(jiffy:encode(ParamK))
	end,Query0,T),
	QueryBase ++ QueryN.
