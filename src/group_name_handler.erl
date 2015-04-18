-module(group_name_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    content_types_accepted/2,
    put_json/2,
    get_json/2,
    is_authorized/2,
    resource_exists/2
    ]).
   
init(Req,State) ->
    {cowboy_rest,Req,State}.
    
allowed_methods(Req,State) ->
    {[<<"HEAD">>,<<"GET">>,<<"PUT">>,<<"OPTIONS">>], Req, State}.
    
content_types_provided(Req, State) ->
    {[{{<<"application">>,<<"json">>,[]}, get_json}], Req, State}.
    
content_types_accepted(Req,State) ->
    {[{{ <<"application">>,<<"json">>,'*' }, put_json}],Req, State}.
    
is_authorized(Req,State) ->
    auth_ball:rest_auth(Req,State).
        
resource_exists(Req,State) ->
    ID = cowboy_req:binding(threadid, Req),
    NotExists = {false,Req,State},
    Exists = fun(NewState) -> {true,Req,NewState} end,
    case db_utils:fetch(ID) of
        {ok,{JSONData}} ->
            case proplists:get_value(<<"name">>,JSONData) of
                undefined ->
                    NotExists;
                _Name ->
                    Exists([{document,{JSONData}}|State])
            end;
        {error,_Err} ->
            NotExists
    end.
     
put_json(Req, State) ->
    {ok, Body, NewReq} = cowboy_req:body(Req),
    Uid = proplists:get_value(user,State),
    NewThreadName = jiffy:decode(Body),
    JSONData = proplists:get_value(document,State),
    Failure = {false,NewReq,State},
    Success = {true,NewReq,State},
    case {JSONData,object_utils:valid_thread_name(NewThreadName)} of 
        {{Thread},true} ->
            case auth_ball:user_in_thread(Uid,JSONData) of
                true -> 
                    NewThread = [{<<"name">>,NewThreadName}|proplists:delete(<<"name">>,Thread)],
                    db_utils:put_to_db({NewThread}),
                    Success;
                false ->
                    Failure
            end;
        _ -> 
            Failure            
    end.

get_json(Req,State) ->
    {Thread} = proplists:get_value(document,State),
    case auth_ball:user_in_thread(State,{Thread}) of
        true ->
            Name = proplists:get_value(<<"name">>,Thread),
            {jiffy:encode(Name),Req,State};
        false ->
            {false,Req,State}
    end.
    
    
