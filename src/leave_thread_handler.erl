-module(leave_thread_handler).
-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    forbidden/2,
    delete_resource/2
    ]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>,<<"DELETE">>,<<"OPTIONS">>], Req, State}.

is_authorized(Req, State) ->
    auth_ball:rest_auth(Req,State).
    
forbidden(Req,State) ->
    auth_ball:forbidden_from_user(Req,State).

delete_resource(Req,Opts) ->
    Thread = cowboy_req:binding(threadid, Req),
    Uid = proplists:get_value(user, Opts),
    % MqttClient = proplists:get_value(mqtt_client, Opts),
    {ok, {ThreadData}} = '3rd-base_db_utils':fetch(Thread),
    UsersInThread = proplists:get_value(<<"users">>, ThreadData),
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
    '3rd-base_db_utils':put_to_db(Thread, {NewThreadData}),
    Topic = << <<"threads/">>/binary, Thread/binary, <<"/members">>/binary>>,
    Client = proplists:get_value(client,Opts),
    message_utils:send_message(Client, Payload,Topic),
    Req2 = cowboy_req:set_resp_body(Payload,Req),
    {true, Req2, Opts}.
