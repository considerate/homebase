-module (message_history_handler).
-define(MAX_MESSAGES,30).
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

get_json(Req, Opts) ->
    Thread = cowboy_req:binding(threadid, Req),
    QS = cowboy_req:parse_qs(Req),
    After = proplists:get_value(<<"after">>,QS),
    Before = proplists:get_value(<<"before">>,QS),

    StartKey = case After of
        undefined ->
            {"startkey",[Thread]};
        _After ->
            {"startkey",[Thread, After]}
    end,

    {QueryParams,QueryOpts} = case Before of
        undefined ->
            EndKey = {"endkey",[Thread,{[]}]},
            {[EndKey],[]};
        _Before -> 
            EndKey = {"endkey",[Thread,Before]},
            Limit = {"limit",?MAX_MESSAGES},
            Decending = {"descending",true},
            {[StartKey,EndKey,Limit,Decending],[reverse]}
    end,

    JSONData = db_utils:query("/_design/messages/_view/message_history",QueryParams,{opts,QueryOpts}),
    Rows = json_utils:get_field(<<"rows">>,JSONData),

    [FirstMessage|_] = Rows,
    LastMessage = lists:last(Rows),

    FirstMessageId = json_utils:get_field(<<"_id">>,FirstMessage),
    LastMessageId = json_utils:get_field(<<"_id">>,LastMessage),

    BaseLink = iolist_to_binary([<<"/threads/">>, Thread, <<"/messages">>]),
    GotMaxMessages = length(Rows) == ?MAX_MESSAGES,

    Links = if GotMaxMessages ->
            case {After,Before} of
                {undefined,undefined} ->
                    none;
                {After,undefined} ->
                    BeforeLink = web_utils:create_query_url(BaseLink,[{<<"before">>,FirstMessageId}]),
                    {[{<<"before">>,BeforeLink}]};
                {_,Before} ->
                    BeforeLink = web_utils:create_query_url(BaseLink,[{<<"before">>,FirstMessageId}]),
                    AfterLink = web_utils:create_query_url(BaseLink,[{<<"after">>,LastMessageId}]),
                    {[{<<"after">>,AfterLink},{<<"before">>,BeforeLink}]}				
            end;
        true ->
            BeforeLink = web_utils:create_query_url(BaseLink,[{<<"before">>,FirstMessageId}]),
            AfterLink = web_utils:create_query_url(BaseLink,[{<<"after">>,LastMessageId}]),
            {[{<<"after">>,AfterLink},{<<"before">>,BeforeLink}]}	
    end,

    JSON = case Links of
        none ->
            JSONData;
        _ ->
            json_utils:add_fields([{<<"links">>,Links}],JSONData)
    end,
    {jiffy:encode(JSON), Req, Opts}.
% links suppplied in different situations
% {undefined,undefined} -> na
% {after,undefined} -> before
% {undefined,before} -> before, after
% {after,before} -> before, after
