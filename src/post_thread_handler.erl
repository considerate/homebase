-module(post_thread_handler).
-export([
    init/2,
    resource_exists/2,
    allowed_methods/2,
    content_types_accepted/2,
    post_json/2,
    is_authorized/2
    ]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

resource_exists(Req, State) ->
    {false, Req, State}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>,<<"POST">>,<<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{ <<"application">>,<<"json">>,'*' }, post_json}], Req, State}.

is_authorized(Req, State) ->
    auth_ball:rest_auth(Req,State).

post_json(Req, State) ->
    {ok, Body, NewReq} = cowboy_req:body(Req),
    {BodyData} = jiffy:decode(Body),

    Uid = proplists:get_value(user, State),
    MqttClient = proplists:get_value(mqtt_client, State),

    Id = objectid_gen_server:objectid(),
    Users = sets:from_list(proplists:get_value(<<"users">>, BodyData)),
    AllUsers = sets:to_list(sets:add_element(Uid, Users)),
    {Result} = db_utils:add_thread(Id, AllUsers, Uid),

    ThreadId = proplists:get_value(<<"_id">>, Result),
    Output = {[
                {id, ThreadId},
                {users, AllUsers},
                {creator, Uid}
                ]},

    JSONOutput = jiffy:encode(object_utils:thread_data(Output)),
    Send = message_utils:send_message(MqttClient,JSONOutput),
    lists:map(Send,AllUsers),

    ResultURL = <<"/threads/", ThreadId/binary>>,
    {{true, ResultURL}, NewReq, State}.

