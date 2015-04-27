-module(my_threads_handler).
-export([
    init/2,
    allowed_methods/2,
    content_types_provided/2,
    get_json/2,
    is_authorized/2,
    forbidden/2
    ]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>,<<"GET">>,<<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>,<<"json">>,[]}, get_json}], Req, State}.

is_authorized(Req, State) ->
    auth_ball:rest_auth(Req,State).

forbidden(Req,State) ->
    auth_ball:forbidden_from_user(Req,State).

get_json(Req,State) ->
    Uid = proplists:get_value(user,State),
    {Props} = db_utils:query("/_design/users/_view/threads", Uid),
    Threads = proplists:get_value(<<"rows">>, Props),
    Response = {[{threads, Threads}]},
    {jiffy:encode(Response), Req, State}.

