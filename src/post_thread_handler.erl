-module(post_thread_handler).
-export([init/2]).

init(Req, Opts) ->
   case auth_ball:authenticate(Req) of
		{ok, Data} -> 
			{ok, Body, Req2} = cowboy_req:body(Req),
			{BodyData} = jiffy:decode(Body),
			Uid = proplists:get_value(<<"id">>, Data),
			{Id, NextId} = proplists:get_value(objectid, Opts),
			MqttClient = proplists:get_value(mqtt_client, Opts),
			NewOpts = [{objectid, NextId()} | proplists:delete(objectid, Opts)],
			Users = sets:from_list(proplists:get_value(<<"users">>, BodyData)),
			AllUsers = sets:to_list(sets:add_element(Uid, Users)),
			Output = db_utils:add_thread(Id, AllUsers, Uid),
			JSONOutput = jiffy:encode(Output),
			Topics = lists:map(fun(User) ->
					iolist_to_binary([<<"users/">>, User, <<"/newthread">>])
			 	end,
			 	AllUsers
			),
			lists:map(fun(Topic) ->
					mqtt_client:send(
						MqttClient, 
						mqtt:publish([{payload, JSONOutput}, {topic, Topic}])
					)
				end,
			Topics),
			web_utils:respond_created(Req2, Output, NewOpts);
		Error ->
			web_utils:respond_forbidden(Req,Error)
		end.