-module(post_thread_handler).
-export([
    init/2,
    resource_exists/2,
    allowed_methods/2,
    content_types_accepted/2,
    post_json/2,
    is_authorized/2,
    new_resource/2
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
    Users = sets:from_list(proplists:get_value(<<"users">>, BodyData)),
    AllUsers = sets:to_list(sets:add_element(Uid, Users)),
    case web_utils:is_blocked(Uid,Users) of
    	true ->
    	   {false, Req, State};
    	false ->
        case AllUsers of
		[] -> {false, NewReq,State};
		[_User1] -> {false, NewReq,State};
		AllUsers ->
		    {Thread,OutputObj} = case AllUsers of
		        [User1,User2] ->
		            Prefix = <<"chat">>,
		            Sep = <<"_">>,
		            [FirstUser,SecondUser] = lists:sort([User1,User2]),%Makes sure name doesn't depend of who creates the thread
		            ThreadId = <<Prefix/binary,
		                   Sep/binary,
		                   FirstUser/binary,
		                   Sep/binary,
		                   SecondUser/binary>>,
		            Private = true,
		            case db_utils:add_thread(ThreadId,AllUsers,Uid,Private) of
		                {error, _Error} ->
		                    {ThreadId,other};
		                {ok, _} ->
		                    Output = {  [
		                                {id, ThreadId},
		                                {users,AllUsers},
		                                {creator, Uid},
		                                {private, Private}
		                                ]},
		                    {ThreadId,Output}
		            end;
		        AllUsers ->
		            ThreadIdOid = objectid_gen_server:objectid(),
		            ThreadId = objectid:bin_to_hex(ThreadIdOid),
		            Private = false,
		            {ok, _} = db_utils:add_thread(ThreadId, AllUsers, Uid, Private),
		            Output = {[
		                {id, ThreadId},
		                {users, AllUsers},
		                {creator, Uid},
		                {private, Private}
		            ]},
		            {ThreadId, Output}
		    end,
		    ResultURL = <<"/threads/", Thread/binary>>,
		    case OutputObj of
		        other ->
		            NewState = [{exists,true}|State],
		            {{true, ResultURL}, NewReq, NewState};
		        OutputObj ->
		            JSONOutput = jiffy:encode(OutputObj),
		            Send = message_utils:send_message(MqttClient,JSONOutput),
		            lists:map(Send,AllUsers),
		            {{true, ResultURL}, NewReq, State}
		    end
	    end
    end.

new_resource(Req,State) ->
    Exists = proplists:get_bool(exists,State),
    {not Exists, Req, State}.

