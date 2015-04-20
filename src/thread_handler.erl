-module(thread_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    get_json/2,
    is_authorized/2,
    resource_exists/2,
    forbidden/2
    ]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>,<<"GET">>,<<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>,<<"json">>,[]}, get_json}], Req, State}.

is_authorized(Req, State) ->
    auth_ball:rest_auth(Req,State).
    
forbidden(Req,State) ->
	auth_ball:user_forbidden_from_thread(Req,State).
	
resource_exists(Req, State) ->
    case proplists:get_value(document,State) of
        none ->
            {false,Req,State};
        {_Thread} ->
            {true,Req,State}
    end.
    
get_json(Req,State) ->
    JSONData = proplists:get_value(document,State),
    ThreadData = object_utils:thread_data(JSONData),
    BodyText = jiffy:encode({[{thread,ThreadData}]}),
    {BodyText, Req, State}.

    
    
    
    
    
    
    
