-module (message_history_handler).
-define(MAX_MESSAGES,20).
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

get_query_params(Thread,Before,After) ->
    Limit = {limit,?MAX_MESSAGES},
    Skip = {skip,1}, %Do not include the id in before or after
    case {Before,After} of
        {undefined,undefined} ->
            StartKey = {startkey,[Thread,{[]}]},
            EndKey = {endkey,[Thread]},
            Descending = {descending,true},
            {[Limit,StartKey,EndKey,Descending],[reverse]}
            ;
        {undefined, After} ->
            StartKey = {startkey,[Thread, After]},
            EndKey = {endkey,[Thread,{[]}]},
            {[Limit,Skip,StartKey,EndKey],[]}
            ;
        {Before, undefined} ->
            StartKey = {startkey,[Thread,Before]},
            EndKey = {endkey,[Thread]},
            Descending = {descending,true},
            {[Limit,Skip,StartKey,EndKey,Descending],[reverse]}
            ;
        {Before, After} ->
            StartKey = {startkey,[Thread, After]},
            EndKey = {endkey,[Thread,Before]},
            {[Limit,Skip,StartKey,EndKey],[]}
    end.

get_pagination_links(BaseLink,Rows,Before,After) ->
    JSONEncode = false,
    CreateLink = fun(Key,Id) ->
            {Key, web_utils:create_query_url(BaseLink,[{Key,Id}],JSONEncode)}
    end,
    case length(Rows) of
        ?MAX_MESSAGES ->
            [{FirstMessage}|_] = Rows,
            {LastMessage} = lists:last(Rows),
            LastId = proplists:get_value(<<"id">>, LastMessage),
            FirstId = proplists:get_value(<<"id">>, FirstMessage),
            case {After,Before} of
                {undefined,undefined} ->
                    [CreateLink(<<"before">>,FirstId)];
                {After,undefined} ->
                    [CreateLink(<<"after">>,LastId)];
                {undefined,Before} ->
                    [CreateLink(<<"before">>,FirstId)];
                {After,Before} ->
                    [CreateLink(<<"before">>,FirstId),CreateLink(<<"after">>,LastId)]
            end;
        _NotMaxRows ->
            []
    end.

get_json(Req, Opts) ->
    Thread = cowboy_req:binding(threadid, Req),
    QS = cowboy_req:parse_qs(Req),
    After = proplists:get_value(<<"after">>,QS),
    Before = proplists:get_value(<<"before">>,QS),
    {QueryParams,QueryOpts} = get_query_params(Thread,Before,After),
    JSONData = db_utils:query("/_design/messages/_view/message_history",QueryParams,{opts,QueryOpts}),
    Rows = json_utils:get_field(<<"rows">>,JSONData),
    BaseLink = iolist_to_binary([<<"/threads/">>, Thread, <<"/messages">>]),
    Links = get_pagination_links(BaseLink,Rows,Before,After),
    JSON = {[{messages, Rows}]},
    NewJSON = json_utils:add_fields([{links,{Links}}],JSON),
    {jiffy:encode(NewJSON), Req, Opts}.
