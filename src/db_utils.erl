-module (db_utils).
-export ([query/1, query/2, query/3, put_to_db/2, add_thread/4, get_row_value/1, fetch/1]).
-define(BASE_ADDRESS,"http://localhost:5984/baseball").

fetch(Id) when is_binary(Id) ->
    fetch(binary_to_list(Id));
fetch(Id) when is_list(Id)->
    {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, Body}} = httpc:request(?BASE_ADDRESS ++ "/" ++ Id),
    case StatusCode of
    	200 ->
    		{ok,jiffy:decode(Body)};
    	Err ->
    		{error,Err}
    end.

query(Query)->
    query(Query,{opts,[]}).

query(Query, {opts,Opts}) when is_binary(Query) ->
    NewQuery = binary_to_list(Query),
    query(NewQuery,{opts,Opts});
query(Query,{opts,Opts}) when is_list(Query) ->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(?BASE_ADDRESS ++ Query),
    {Data} = jiffy:decode(Body),
    Rows = proplists:get_value(<<"rows">>, Data),
    NewRows = case proplists:get_bool(reverse,Opts) of
        false ->
            Rows;
        true ->
            lists:reverse(Rows)
    end,
    {[{<<"rows">>,lists:map(fun get_row_value/1, NewRows)}]};
query(QueryBase,[{Key,Param}|Rest]) ->
    query(QueryBase,[{Key,Param}|Rest], {opts,[]});
query(Query,Key) ->
    query(Query,[{"key",Key}]).

query(BasePath, [{Key,Param}|Rest], {opts,Opts}) ->
    AllParams = [{Key,Param}|Rest],
    QS = binary_to_list(web_utils:querystring(AllParams)),
    query(BasePath ++ QS, {opts, Opts});
query(Query,StartKey,EndKey) ->
    Start = binary_to_list(jiffy:encode(StartKey)),
    End = binary_to_list(jiffy:encode(EndKey)),
    query(Query, [{"startkey",Start},{"endkey",End}]).

add_thread(Id,Users,Creator,Private) ->
    Output = {[
                {<<"type">>,<<"thread">>},
                {<<"_id">>,Id},
                {<<"users">>,Users},
                {<<"creator">>,Creator},
                {<<"private">>, Private}
                ]},
    put_to_db(Id,Output).


put_to_db(Id,StuffsToAdd) ->
    % Specifying options for http request to db
    Method = put,
    Url = ?BASE_ADDRESS ++ "/" ++ binary_to_list(Id),
    Headers = [],
    Content_type = "application/json",
    Body = jiffy:encode(StuffsToAdd),
    Request = {Url,Headers,Content_type,Body},
    HTTPOptions = [],
    Options = [],

    %Making request to db
    httpc:request(Method,Request,HTTPOptions,Options).

get_row_value({Obj}) ->
    Value = proplists:get_value(<<"value">>, Obj),
    Value.
