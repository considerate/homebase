-module(message_utils).
-export([send_message/3, send_message/4]).

send_message(Client,Payload, TopicFn, Users) when is_function(TopicFn) ->
   lists:map(fun(User) ->
        Topic = TopicFn(User),
        send_message(Client,Payload,Topic)
   end, Users).
send_message(Client, Payload,Topic) ->
    mqtt_client:send(Client,mqtt:publish([{topic,Topic},{payload,Payload}])).
