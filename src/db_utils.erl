-module (db_utils).
-export ([query/1, query/2, query/3, put_to_db/2, add_thread/3, get_row_value/1, fetch/1]).
-define(BASE_ADDRESS,"http://localhost:5984/baseball").

fetch(Id) when is_binary(Id) ->
    fetch(binary_to_list(Id));
fetch(Id) when is_list(Id)->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(?BASE_ADDRESS ++ "/" ++ Id),
    jiffy:decode(Body).
query(Query)->
    query(Query,{opts,[]}).

%Opts must be a proptylists
%Options:
%	reverse: reverses the order of the recived documents from the database.
query(Query,{opts,Opts}) ->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(?BASE_ADDRESS ++ Query),
    {Data} = jiffy:decode(Body),
    {_, Rows0} = proplists:lookup(<<"rows">>, Data),
    Rows = case proplists:get_bool(reverse,Opts) of
        false -> 
            Rows0;
        true ->
            list:reverse(Rows0)
    end,
    {[{<<"rows">>,lists:map(fun get_row_value/1, Rows)}]};

%queies cdb with a base quary and a list of supplied query parameters.
%Key must be a string
%Param must be parsable by jiffy

query(QueryBase,[{Key0,Param0}|T]) ->
    query(QueryBase,[{Key0,Param0}|T], {opts,[]})
    ;
query(Query,Key) ->
    query(Query,[{"key",Key}]).

query(QueryBase, [{Key0,Param0}|T], {opts,Opts}) ->	
    Query0 = "?" ++ Key0 ++ "=" ++ binary_to_list(jiffy:encode(Param0)),
    QueryN = lists:foldl(
            fun({KeyK,ParamK},QueryK) ->
                    QueryK ++ "&" ++ KeyK ++ "=" ++ binary_to_list(jiffy:encode(ParamK))
            end,Query0,T),
    query(QueryBase ++ QueryN,{opts,Opts})
    ;		
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

%get_messages(Group,{time,StartHour,StartMinut,StartSecond},{time,EndHour,EndMinut,EndSecond}) ->
%	query("/_design/Messages/_view/message_history?startkey =\"">>,jiffy:encode([Group,[StartHour,StartMinut,StartSecond]]) ++ "\"&endkey=\""++ "[" ++ Group ++ ",[" ++ EndHour ++ "," ++ EndMinut ++ "," ++ EndSecond ++ "]" ++ "]" ++ "]" ++ "\"").

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
    proplists:get_value(<<"value">>, Obj).

%The above erlang datastructure corresponds to the following json
%"{ 
%
%			  "Subject":"I like Plankton",
%			  "Author":"Rusty",
%			  "PostedDate":"2006-08-15T17:30:12-04:00",
%			  "Tags":["plankton", "baseball", "decisions"],
%			  "Body":"I decided today that I don't like baseball. I like plankton."
%			}"
%	).
