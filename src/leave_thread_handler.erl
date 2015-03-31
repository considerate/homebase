-module(leave_thread_handler).
-export([init/2]).

init(Req,Opts) ->
    case auth_ball:authenticate(Req) of
		{ok, Data} -> 
			Thread = cowboy_req:binding(threadid, Req),
			UserToRemove = cowboy_req:binding(userid, Req),
			Uid = proplists:get_value(<<"id">>, Data),
            MqttClient = proplists:get_value(mqtt_client, Opts),
            JSONData = db_utils:fetch(Thread),
            {ThreadData} = JSONData,
            UsersInThread = proplists:get_value(<<"users">>, ThreadData),
            IsInThread = lists:any(fun(X) ->  Uid =:= X end, UsersInThread),
			case Uid =:= UserToRemove andalso IsInThread of
				true ->
					UsersInThreadSet = sets:from_list(UsersInThread),
					NewUsers = sets:to_list(sets:del_element(UserToRemove, UsersInThreadSet)),
					NewThreadData = [{<<"users">>, NewUsers}| proplists:delete(<<"users">>, ThreadData)],
                    Output = {[
                        {id, Thread},
                        {removed, [UserToRemove]},
                        {users, NewUsers}
                    ]},
                    Payload = jiffy:encode(Output),
					db_utils:put_to_db(Thread, {NewThreadData}),
                    Send = message_utils:send_message(MqttClient,Payload),
                    lists:map(Send, NewUsers),
					web_utils:respond_success(Req, Output, Opts);
				false ->
					web_utils:respond_forbidden(Req, error)
			end;
		Error ->
			web_utils:respond_forbidden(Req, Error)
	end. 
