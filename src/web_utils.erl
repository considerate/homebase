-module(web_utils).
-export([respond_resource_not_found/2,respond_success/3]).

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
