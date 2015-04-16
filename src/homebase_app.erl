-module(homebase_app).
-behaviour(application).
-export([start/2,stop/1]).

-define(ONE_WEEK, 604800).

start(_Type, _Args) ->
    application:ensure_all_started(lager),
    io:format("Homebase: ~p~n",[self()]),
    objectid_gen_server:start_link(),
    {ok, Client} =  mqtt_client_simple:connect([{client_id,<<"admin">>}]),
    MqttOptions = [{client,Client}],
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", hello_handler, []},
            {"/threads/:threadid/messages", message_history_handler, []},
            {"/users/:userid/threads", my_threads_handler,[]},
            {"/threads", post_thread_handler, MqttOptions},
            {"/threads/:threadid", thread_handler,[]},
            {"/threads/:threadid/users", add_users_to_thread_handler, MqttOptions},
            {"/threads/:threadid/users/:userid", leave_thread_handler, MqttOptions}
        ]}
    ]),
    {ok, BindAddress} = inet:parse_ipv4_address("0.0.0.0"),
    Port = 8088,
    io:format("Address to listen on: ~p:~p~n", [BindAddress,Port]),
    Cowboy = cowboy:start_http(homebase_http, 100,
        [{port, Port},
         {ip, BindAddress}],
        [{env, [{dispatch, Dispatch}]},
         {middlewares, [cowboy_router,
                        homebase_cors,
                        cowboy_handler]}]
    ),
    io:format("Cowboy statrted as ~p~n", [Cowboy]),
    homebase_sup:start_link().

stop(_State) ->
    ok.
