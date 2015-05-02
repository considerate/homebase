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
    Uid = proplists:get_value(user, State),
    {ok, Body, NewReq} = cowboy_req:body(Req),
    {BodyData} = jiffy:decode(Body),
    Users = sets:from_list(proplists:get_value(<<"users">>, BodyData)),
    AllUsers = sets:to_list(sets:add_element(Uid, Users)),
    ThreadName = proplists:get_value(<<"name">>,BodyData),
    Failure = {false, Req, State},
    Success = fun(NewState,ThreadId) ->
        ResultURL = <<"/threads/", ThreadId/binary>>,
        {{true, ResultURL}, NewReq, NewState}
    end,
    case web_utils:is_blocked(Uid,Users) of
        true ->
           Failure;
        false ->
            case AllUsers of
                [] -> 
                    Failure;
                [_User] ->
                    Failure;
                [User1,User2] ->
                    Prefix = <<"chat">>,
                    Sep = <<"_">>,
                    [FirstUser,SecondUser] = lists:sort([User1,User2]),%Makes sure name doesn't depend of who creates the thread
            	    ThreadId =  <<Prefix/binary,
                                Sep/binary,
                                FirstUser/binary,
                                Sep/binary,
                                SecondUser/binary>>,
                    Private = true,
                    case '3rd-base_db_utils':add_thread(ThreadId,AllUsers,ThreadName,Uid,Private) of
                        {error, _Error} ->
                            NewState = [{exists,true}|State],
                            Success(NewState,ThreadId);
                        {ok, _} ->
                            message_utils:send_new_thread(State,ThreadId,AllUsers,ThreadName,Uid,Private),
                            Success(State,ThreadId)
                    end;
                AllUsers ->
                    ThreadIdOid = objectid_gen_server:objectid(),
                    ThreadId = objectid:bin_to_hex(ThreadIdOid),
                    Private = false,
                    {ok, _} = '3rd-base_db_utils':add_thread(ThreadId, AllUsers,ThreadName, Uid, Private),
                    message_utils:send_new_thread(State,ThreadId,AllUsers,ThreadName,Uid,Private),
                    Success(State,ThreadId)
            end
    end.

new_resource(Req,State) ->
    Exists = proplists:get_bool(exists,State),
    {not Exists, Req, State}.
