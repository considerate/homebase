-module(homebase_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-define(ONE_WEEK, 604800).

start(_Type, _Args) ->
    % Change the configuration of baseball by changing the tupple bellow
    objectid_gen_server:start_link(),
    mqtt_client_sup:start_link(),

    Payload = {[{admin,true}]},
    AdminToken = ejwt:jwt(<<"HS256">>, Payload, ?ONE_WEEK, auth_ball:secret()),
    MqttParams = [{username, <<"admin">>},
                  {password, AdminToken},
                  {client_id, objectid_gen_server:objectid()}],
    {ok, MQTTClient} = mqtt_client_simple:connect(MqttParams),
    MqttOptions = [{objectid, objectid_gen_server:objectid()}, {mqtt_client,MQTTClient}],

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", hello_handler, []},
            {"/threads/:threadid/messages", message_history_handler, []},
            {"/users/:userid/threads", my_threads_handler,[]},
            {"/threads", post_thread_handler, []},
            {"/threads/:threadid", thread_handler,[]},
            {"/threads/:threadid/users", add_users_to_thread_handler, MqttOptions},
            {"/threads/:threadid/users/:userid", leave_thread_handler, MqttOptions},
            {"/threads", post_thread_handler, MqttOptions},
            {"/threads/:threadid", thread_handler,[]}
        ]}
    ]),
    {ok, BindAddress} = inet:parse_ipv4_address("0.0.0.0"),
    Port = 8088,
    io:format("Address to listen on: ~p:~p", [BindAddress,Port]),
    cowboy:start_http(http, 100,
        [{port, Port},{ip, BindAddress}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    homebase_sup:start_link().

stop(_State) ->
    ok.
