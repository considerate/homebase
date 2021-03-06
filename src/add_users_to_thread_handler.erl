-module(add_users_to_thread_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    post_json/2,
    is_authorized/2
    ]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>,<<"POST">>,<<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{ <<"application">>,<<"json">>,'*' }, post_json}], Req, State}.

is_authorized(Req, State) ->
    auth_ball:rest_auth(Req,State).

post_json(Req,Opts) ->
    Thread = cowboy_req:binding(threadid, Req),
    Uid = proplists:get_value(user, Opts),
    {ok,{ThreadData}} = '3rd-base_db_utils':fetch(Thread),
    UsersInThread = proplists:get_value(<<"users">>, ThreadData),
    IsInThread = lists:member(Uid, UsersInThread),
    IsPrivate = proplists:get_bool(<<"private">>,ThreadData),
    case IsInThread andalso (not IsPrivate) of
        true ->
            {ok, Body, Req2} = cowboy_req:body(Req),
            {BodyData} = jiffy:decode(Body),
            UsersToAdd = sets:from_list(proplists:get_value(<<"users">>, BodyData)),
            case web_utils:is_blocked(Uid,UsersToAdd) of
                false ->
                    UsersInThreadSet = sets:from_list(UsersInThread),
                    NewUsers = sets:to_list(sets:union(UsersToAdd, UsersInThreadSet)),
                    NewThreadData = [{<<"users">>, NewUsers}| proplists:delete(<<"users">>, ThreadData)],
                    Output = {[
                                {users, NewUsers},
                                {added, sets:to_list(UsersToAdd)},
                                {id, Thread}
                                ]},
                    Payload = jiffy:encode(Output),
                    '3rd-base_db_utils':put_to_db(Thread, {NewThreadData}),
                    MqttClient = proplists:get_value(client, Opts),
                    Topic = << <<"threads/">>/binary, Thread/binary, <<"/members">>/binary >>,
                    message_utils:send_message(MqttClient,Payload, Topic),
                    ResultURL = <<"/threads/", Thread/binary>>,
                    {{true,ResultURL}, Req2, Opts};
                true ->
                     {false,Req,Opts}
            end;
        false ->
            {false, Req, Opts}
    end.
