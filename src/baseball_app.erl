-module(baseball_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).


start(_Type, _Args) ->
	%Change the configuration of baseball by changing the tupple bellow
    random:seed(erlang:now()),
    ObjectId = objectid:objectid(),
    Dispatch = cowboy_router:compile([
        {'_', [
        	{"/", hello_handler, []},
            {"/threads/:threadid/messages", message_history_handler, []},
        	{"/users/me/threads", my_threads_handler,[]},
            {"/threads", post_thread_handler, [{objectid, ObjectId}]},
            {"/threads/:threadid", thread_handler,[]},
            {"/threads/:threadid/users", add_users_to_thread_handler, []}
        ]}
    ]),
    {ok, BindAddress} = inet:parse_ipv4_address("0.0.0.0"),
    Port = 8088,
    io:format("Address to listen on: ~p:~p", [BindAddress,Port]),
    cowboy:start_http(my_http_listener, 100,
        [{port, Port},{ip, BindAddress}],
        [{env, [{dispatch, Dispatch}]}]
    ),
	baseball_sup:start_link().
	

stop(_State) ->
	ok.
