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

is_blocked(Uid,UsersToAdd) ->
    {ok,Settings} = application:get_env(homebase,user_api),
    BasePath = proplists:get_value(base_path,Settings),
    URL = BasePath ++ "/users/" ++ binary_to_list(Uid),
    {ok,{_,	_,UserBody}} = httpc:request(URL),
    {UserData} = jiffy:decode(UserBody),
    Blocking = proplists:get_value(<<"blocking">>,UserData),
    lists:any(fun(Blocked) -> sets:is_element(Blocked, UsersToAdd) end, Blocking).
    
post_json(Req,Opts) ->
    Thread = cowboy_req:binding(threadid, Req),
    Uid = proplists:get_value(user, Opts),
    MqttClient = proplists:get_value(mqtt_client, Opts),
    {ThreadData} = db_utils:fetch(Thread),
    UsersInThread = proplists:get_value(<<"users">>, ThreadData),
    IsInThread = lists:member(Uid, UsersInThread),
    IsPrivate = proplists:get_bool(<<"private">>,ThreadData),
    case IsInThread andalso (not IsPrivate) of
        true ->
            {ok, Body, Req2} = cowboy_req:body(Req),
            {BodyData} = jiffy:decode(Body),
            UsersToAdd = sets:from_list(proplists:get_value(<<"users">>, BodyData)),
            UsersInThreadSet = sets:from_list(UsersInThread),
            case is_blocked(Uid,UsersToAdd) of
                false ->  
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
                    ResultURL = <<"/threads/", Thread/binary>>,
                    {{true,ResultURL}, Req2, Opts};
                true -> 
                     {false,Req,Opts}
            end;
        false ->
            {false, Req, Opts}
    end.
