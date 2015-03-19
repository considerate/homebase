-module(post_thread_handler).
-export([init/2]).

init(Req, Opts) ->

   case auth_ball:authenticate(Req) of
		{ok, Data} -> 
			{ok, Body, Req2} = cowboy_req:body(Req),
			{BodyData} = jiffy:decode(Body),
			Uid = proplists:get_value(<<"id">>, Data),
			Users = proplists:get_value(<<"users">>, BodyData),
			db_utils:add_thread(<<"AOSENUH49GGEO">>,Uid, Users)
		end.