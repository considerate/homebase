-module(post_thread_handler).
-export([init/2]).

init(Req, Opts) ->
   case auth_ball:authenticate(Req) of
		{ok, Data} -> 
			{ok, Body, Req2} = cowboy_req:body(Req),
			{BodyData} = jiffy:decode(Body),
			Uid = proplists:get_value(<<"id">>, Data),
			{objectid,{Id, NextId}} = proplists:lookup(objectid, Opts),
			NewOpts = [{objectid, NextId()} | proplists:delete(objectid, Opts)],
			Users = sets:from_list(proplists:get_value(<<"users">>, BodyData)),
			AllUsers = sets:add_element(Uid, Users),
			Output = db_utils:add_thread(Id, sets:to_list(AllUsers), Uid),
			% sets:map(fun(User) ->
				
			% 	end,
			% 	AllUsers
			% ),
			web_utils:respond_created(Req2, Output, NewOpts);
		Error ->
			web_utils:respond_forbidden(Req,Error)
		end.