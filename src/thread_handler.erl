-module(thread_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    get_json/2,
    is_authorized/2
    ]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>,<<"GET">>,<<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>,<<"json">>,[]}, get_json}], Req, State}.

is_authorized(Req, State) ->
    auth_ball:rest_auth(Req,State).

get_json(Req,Opts) ->
    Thread = cowboy_req:binding(threadid, Req),
    {ok,JSONData} = db_utils:fetch(Thread),
    ThreadData = object_utils:thread_data(JSONData),
    case auth_ball:user_in_thread(Opts,JSONData) of
        true ->
            BodyText = jiffy:encode({[{thread,ThreadData}]}),
            {BodyText, Req, Opts};
        false ->
            {false, Req, Opts}
    end.
    
    
    
    
    
    
    
