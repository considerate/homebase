-module(message_utils).
-export([send_message/2]).



send_message(MqttClient,Payload) ->
    fun(User) ->
            Topic = list_to_binary([<<"users/">>, User, <<"/newthread">>]),
            mqtt_client:send(MqttClient,mqtt:publish([{payload,Payload}, {topic,Topic}]))
    end.
