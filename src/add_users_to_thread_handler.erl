-module(add_users_to_thread_handler).
-export([init/2]).

init(Req,Opts) ->
    case auth_ball:authenticate(Req) of
		{ok, Data} -> 
			Thread = cowboy_req:binding(threadid, Req),
			Uid = proplists:get_value(<<"id">>, Data),
			JSONData = db_utils:fetch(Thread),
			{ThreadData} = JSONData,
			UsersInThread = proplists:get_value(<<"users">>, ThreadData),
			IsInThread = lists:any(fun(X) ->  Uid =:= X end, UsersInThread),
			case IsInThread of
				true ->
					{ok, Body, Req2} = cowboy_req:body(Req),
					{BodyData} = jiffy:decode(Body),
					UsersToAdd = sets:from_list(proplists:get_value(<<"users">>, BodyData)),
					UsersInThreadSet = sets:from_list(UsersInThread),
					NewUsers = sets:to_list(sets:union(UsersToAdd, UsersInThreadSet)),
					NewThreadData = [{<<"users">>, NewUsers}| proplists:delete(<<"users">>, ThreadData)],
					Creator = proplists:get_value(<<"users">>, ThreadData),
					db_utils:put_to_db(Thread, {NewThreadData}),
					web_utils:respond_success(Req2, {NewThreadData}, []);
				false -> 
					web_utils:respond_forbidden(Req, error)
			end;
		Error ->
			web_utils:respond_forbidden(Req, Error)
	end. 