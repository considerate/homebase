-module(web_utils).
-export([respond_forbidden/2, respond_resource_not_found/1,respond_success/3, respond_created/3]).

respond_forbidden(Req,Error) ->
	io:format("ERROR: ~p", [Error]),
	cowboy_req:reply(401,Req).

respond_resource_not_found(Req) ->
	cowboy_req:reply(404,Req).
	
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
