-module(message_utils).
-export([send_message/3, send_message/4,send_new_thread/6,send_new_thread_name/3]).

send_message(Client,Payload, TopicFn, Users) when is_function(TopicFn) ->
   lists:map(fun(User) ->
        Topic = TopicFn(User),
        send_message(Client,Payload,Topic)
   end, Users).
send_message(Client, Payload,Topic) ->
    mqtt_client:send(Client,mqtt:publish([{topic,Topic},{payload,Payload}])).
    
send_new_thread(State,ThreadId,Users,Name,Creator,Private) -> 
    Client = proplists:get_value(client,State),
    BaseOutputObj = 
                [
                    {id, ThreadId},
                    {users, Users},
                    {creator, Creator},
                    {private, Private}
                ],
    OutputObj = case object_utils:valid_thread_name(Name) of
        true ->
            [{<<"name">>,Name}|BaseOutputObj];
        false ->
            BaseOutputObj
    end,
    Payload = jiffy:encode({OutputObj}),
    Topic = fun(User) ->
        << <<"users/">>/binary, User/binary, <<"/newthreads">>/binary >>
    end,
    send_message(Client,Payload,Topic,Users).
    
    
send_new_thread_name(State,ThreadId,Name) ->
     Client = proplists:get_value(client,State),
     Payload = jiffy:encode({[{name,Name}]}),
     Topic = << <<"threads/">>/binary, ThreadId/binary, <<"/name">>/binary >>,
     send_message(Client,Payload,Topic).
    
        
