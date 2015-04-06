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
    Uid = proplists:get_value(user,State),
    case web_utils:get_user_id(Req,State) of
        Uid -> auth_ball:rest_auth(Req,State); % Require id to equal current user
        _ -> {{false, <<":userid">>}, Req,State}
    end.

delete_resource(Req,Opts) ->
    Thread = cowboy_req:binding(threadid, Req),
    Uid = proplists:get_value(user, Opts),
    MqttClient = proplists:get_value(mqtt_client, Opts),
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
    Send = message_utils:send_message(MqttClient,Payload),
    lists:map(Send, NewUsers),
    {true, Req, Opts}.
