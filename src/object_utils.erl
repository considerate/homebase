-module(object_utils).
-export([thread_data/1,valid_thread_name/1]).

thread_data({ThreadData}) ->
    BaseObj = [
        {id, proplists:get_value(<<"_id">>,ThreadData)},
        {users, proplists:get_value(<<"users">>, ThreadData)},
        {creator, proplists:get_value(<<"creator">>,ThreadData)}
    ],
    RetObj = case proplists:get_value(<<"name">>,ThreadData) of
        undefined ->
            BaseObj;
        Name ->
            [Name|BaseObj]
    end,
    {RetObj}.

valid_thread_name(Name) ->
    Name =/= <<"">> andalso is_bitstring(Name).
