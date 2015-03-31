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
            {Result} = db_utils:add_thread(Id, AllUsers, Uid),
            Output = {[
                {id, proplists:get_value(<<"_id">>,Result)},
                {users, proplists:get_value(<<"users">>,Result)},
                {creator, proplists:get_value(<<"creator">>,Result)}
            ]},
			JSONOutput = jiffy:encode(object_utils:thread_data(Output)),
            Send = message_utils:send_message(MqttClient,JSONOutput),
			lists:map(Send,AllUsers),
			web_utils:respond_created(Req2, Output, NewOpts);
		Error ->
			web_utils:respond_forbidden(Req,Error)
		end.


