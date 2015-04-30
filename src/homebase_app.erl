-module(homebase_app).
-behaviour(application).
-export([start/2,stop/1]).

-define(ONE_WEEK, 604800).

start(_Type, _Args) ->
    lager:start(),
    lager:info("Homebase: ~p",[self()]),
    application:start(crypto),
    application:start(asn1),
    application:start(ssl),
    objectid_gen_server:start_link(),
    Payload = {[{admin,true}]},
    AdminToken = ejwt:jwt(<<"HS256">>, Payload, ?ONE_WEEK, auth_ball:secret()),
    MqttParams = [{username, <<"admin">>},
                  {password, AdminToken},
                  {client_id, objectid_gen_server:objectid()}],
    {ok, Client} =  mqtt_client_simple:connect(MqttParams),
    MqttOptions = [{client,Client}],
    MqttDispatch = proplists:get_value(dispatch, fubar:settings(fubar_app)),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", hello_handler, []},
            {"/mqtt", websocket_protocol, [{dispatch, MqttDispatch}]},
            {"/threads/:threadid/messages", message_history_handler, []},
            {"/users/:userid/threads", my_threads_handler,[]},
            {"/threads", post_thread_handler, MqttOptions},
            {"/threads/:threadid", thread_handler,[]},
            {"/threads/:threadid/users", add_users_to_thread_handler, MqttOptions},
            {"/threads/:threadid/users/:userid", leave_thread_handler, MqttOptions},
            {"/threads/:threadid/name",group_name_handler,MqttOptions}
        ]}
    ]),
    {ok, BindAddress} = inet:parse_ipv4_address("0.0.0.0"),
    {ok, Port} = application:get_env(homebase, http_port),
    lager:info("Address to listen on: ~p:~p", [BindAddress,Port]),
    Cowboy = cowboy:start_http(homebase_http, 100,
        [{port, Port},
         {ip, BindAddress}],
        [{env, [{dispatch, Dispatch}]},
         {middlewares, [cowboy_router,
                        homebase_cors,
                        cowboy_handler]}]
    ),
    lager:info("Cowboy started as ~p", [Cowboy]),
    homebase_sup:start_link().

stop(_State) ->
    ok.
