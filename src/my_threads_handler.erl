-module(my_threads_handler).
-export([init/2]).


init(Req,Opts) ->
    case auth_ball:authenticate(Req) of
		{ok, Data} -> 
			Uid = proplists:get_value(<<"id">>, Data),
			JSONData = db_utils:query("/_design/users/_view/threads", Uid),
			web_utils:respond_success(Req, JSONData, Opts);
		Error ->
			io:format("ERROR: ~p", [Error]),
			cowboy_req:reply(401,Req)
	end.
