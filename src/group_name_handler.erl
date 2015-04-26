-module(group_name_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    forbidden/2,
    put_json/2,
    get_json/2,
    is_authorized/2,
    resource_exists/2,
    delete_resource/2
    ]).
   
init(Req,State) ->
    {cowboy_rest,Req,State}.
    
allowed_methods(Req,State) ->
    {[<<"HEAD">>,<<"DELETE">>,<<"GET">>,<<"PUT">>,<<"OPTIONS">>], Req, State}.
    
content_types_provided(Req, State) ->
    {[{{<<"application">>,<<"json">>,[]}, get_json}], Req, State}.
    
content_types_accepted(Req,State) ->
    {[{{ <<"application">>,<<"json">>,'*' }, put_json}],Req, State}.
    
is_authorized(Req,State) ->
    auth_ball:rest_auth(Req,State).

forbidden(Req,State) ->
    case cowboy_req:method(Req) of
        <<"PUT">> ->
            auth_ball:forbidden_from_creator(Req,State);
        <<"DELETE">> ->
            auth_ball:forbidden_from_creator(Req,State);
        _ -> 
            auth_ball:forbidden_from_thread(Req,State)
    end.
    
resource_exists(Req,State) ->
    case proplists:get_value(document,State) of
        {Thread} ->
            case proplists:get_value(<<"name">>,Thread) of
                undefined ->
                    {false,Req,State};
                _Name ->
                    {true,Req,State}
            end;
        none ->
            {false,Req,State}
    end.
     
put_json(Req, State) ->
    {ok, Body, NewReq} = cowboy_req:body(Req),
    {[{<<"name">>,NewThreadName}]} = jiffy:decode(Body),
    JSONData = proplists:get_value(document,State),
    case {JSONData,object_utils:valid_thread_name(NewThreadName)} of 
        {{Thread},true} ->
            NewThread = [{<<"name">>,NewThreadName}|proplists:delete(<<"name">>,Thread)],
            db_utils:put_to_db({NewThread}),
            ThreadId = cowboy_req:binding(threadid, NewReq),
            message_utils:send_new_thread_name(State,ThreadId,NewThreadName),
            {true,NewReq,State};
        _ -> 
            {false,NewReq,State}       
    end.

get_json(Req,State) ->
    {Thread} = proplists:get_value(document,State),
    Name = proplists:lookup(<<"name">>,Thread),
    {jiffy:encode({[Name]}),Req,State}.


delete_resource(Req,State) ->
    {OLDThread} = proplists:get_value(document,State),
    NewThread = proplists:delete(name,OLDThread),
    db_utils:put_to_db({NewThread}),
    ThreadId = cowboy_req:binding(threadid,Req),
    message_utils:send_new_thread_name(State,ThreadId,null),
    {true,Req,State}.
    
