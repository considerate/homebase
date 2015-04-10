-module (db_utils).
-export ([query/1, query/2, query/3, put_to_db/2, add_thread/3, get_row_value/1, fetch/1]).
-define(BASE_ADDRESS,"http://localhost:5984/baseball").

fetch(Id) when is_binary(Id) ->
    fetch(binary_to_list(Id));
fetch(Id) when is_list(Id)->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(?BASE_ADDRESS ++ "/" ++ Id),
    jiffy:decode(Body).

binary_join([First|Binaries], Separator) ->
    lists:foldl(fun(Before, Current) ->
        <<Before/binary, Separator/binary ,Current/binary>>
    end, First, Binaries).

querystring({Key,Param}) when is_list(Key) ->
    Key2 = erlang:list_to_binary(Key),
    querystring({Key2,Param});
querystring({Key,Param}) when is_atom(Key) ->
    Key2 = erlang:atom_to_binary(Key,utf8),
    querystring({Key2,Param});
querystring({Key,Param}) when is_binary(Key) ->
    JSON = jiffy:encode(Param),
    Separator = <<"=">>,
    <<Key/binary, Separator/binary, JSON/binary>>;
querystring([]) ->
    <<>>;
querystring(Params) when is_list(Params) ->
    Binaries = lists:map(fun querystring/1, Params),
    QS = binary_join(Binaries, <<"&">>),
    Question = <<"?">>,
    <<Question/binary, QS/binary>>.

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
            list:reverse(Rows)
    end,
    {[{<<"rows">>,lists:map(fun get_row_value/1, NewRows)}]};
query(QueryBase,[{Key,Param}|Rest]) ->
    query(QueryBase,[{Key,Param}|Rest], {opts,[]});
query(Query,Key) ->
    query(Query,[{"key",Key}]).

query(BasePath, [{Key,Param}|Rest], {opts,Opts}) ->
    AllParams = [{Key,Param}|Rest],
    QS = binary_to_list(querystring(AllParams)),
    query(BasePath ++ QS, {opts, Opts});
query(Query,StartKey,EndKey) ->
    Start = binary_to_list(jiffy:encode(StartKey)),
    End = binary_to_list(jiffy:encode(EndKey)),
    query(Query, [{"startkey",Start},{"endkey",End}]).

add_thread(ThreadID,Users,Creator) ->
    Id = bin_to_hex(ThreadID),
    Output = {[
                {<<"type">>,<<"thread">>},
                {<<"_id">>,Id},
                {<<"users">>,Users},
                {<<"creator">>,Creator}
                ]},
    put_to_db(Id,Output),
    Output.

bin_to_hex(Bin) when is_binary(Bin) ->
    << <<Y>> || <<X:4>> <= Bin, Y <- integer_to_list(X,16) >>.

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
