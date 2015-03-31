-module(add_users_to_thread_handler).
-export([init/2]).

init(Req,Opts) ->
    case auth_ball:authenticate(Req) of
		{ok, Data} -> 
            Thread = cowboy_req:binding(threadid, Req),
            Uid = proplists:get_value(<<"id">>, Data),
            MqttClient = proplists:get_value(mqtt_client, Opts),
            JSONData = db_utils:fetch(Thread),
            {ThreadData} = JSONData,
            UsersInThread = proplists:get_value(<<"users">>, ThreadData),
            IsInThread = lists:any(fun(X) ->  Uid =:= X end, UsersInThread),
            case cowboy_req:method(Req) of
                <<"POST">> ->
                    case IsInThread of
                        true ->
                            {ok, Body, Req2} = cowboy_req:body(Req),
                            {BodyData} = jiffy:decode(Body),
                            UsersToAdd = sets:from_list(proplists:get_value(<<"users">>, BodyData)),
                            UsersInThreadSet = sets:from_list(UsersInThread),
                            NewUsers = sets:to_list(sets:union(UsersToAdd, UsersInThreadSet)),
                            NewThreadData = [{<<"users">>, NewUsers}| proplists:delete(<<"users">>, ThreadData)],
                            Output = {[
                                {users, NewUsers},
                                {added, sets:to_list(UsersToAdd)},
                                {id, Thread}
                            ]},
                            Payload = jiffy:encode(Output),
                            db_utils:put_to_db(Thread, {NewThreadData}),
                            Send = message_utils:send_message(MqttClient,Payload),
                            lists:map(Send, NewUsers),
                            web_utils:respond_created(Req2, Output, Opts);
                        false -> 
                            web_utils:respond_forbidden(Req, error)
                    end
            end;
        Error ->
            web_utils:respond_forbidden(Req, Error)
	end. 
