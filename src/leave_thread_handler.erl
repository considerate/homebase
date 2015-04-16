-module(leave_thread_handler).
-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    delete_resource/2
    ]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>,<<"DELETE">>,<<"OPTIONS">>], Req, State}.

is_authorized(Req, State) ->
    case auth_ball:rest_auth(Req,State) of
        {true, NewReq, NewState} ->
            Uid = proplists:get_value(user,NewState),
            case web_utils:get_user_id(NewReq,NewState) of
                Uid -> {true, NewReq, NewState};
                _ -> {{false, <<":userid">>}, Req,State}
            end;
        Fail -> Fail
    end.

delete_resource(Req,Opts) ->
    Thread = cowboy_req:binding(threadid, Req),
    Uid = proplists:get_value(user, Opts),
    % MqttClient = proplists:get_value(mqtt_client, Opts),
    JSONData = db_utils:fetch(Thread),
    {ThreadData} = object_utils:thread_data(JSONData),
    UsersInThread = proplists:get_value(users, ThreadData),
    true = lists:member(Uid, UsersInThread),
    UsersInThreadSet = sets:from_list(UsersInThread),
    NewUsers = sets:to_list(sets:del_element(Uid, UsersInThreadSet)),
    NewThreadData = [{<<"users">>, NewUsers}| proplists:delete(<<"users">>, ThreadData)],
    Output = {[
                {id, Thread},
                {removed, [Uid]},
                {users, NewUsers}
                ]},
    Payload = jiffy:encode(Output),
    db_utils:put_to_db(Thread, {NewThreadData}),
    Topic = << <<"threads/">>/binary, Thread/binary, <<"/members">>/binary>>,
    Client = proplists:get_value(client,Opts),
    message_utils:send_message(Client,Topic, Payload),
    Req2 = cowboy_req:set_resp_body(Payload,Req),
    {true, Req2, Opts}.
