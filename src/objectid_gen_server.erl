-module(objectid_gen_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([objectid/0]).
-export([init/1,
         code_change/3,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         terminate/2]).

-record(state, {channels, objectid}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

objectid() ->
    gen_server:call(?MODULE, objectid).

init(_Args) ->
    random:seed(erlang:now()),
    {ok, #state{objectid=objectid:objectid()}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call(objectid, _From, #state{objectid={Id, NextId}}) ->
    {reply, Id, #state{objectid=NextId()}}.

handle_info({_Code, _Pid, _Reason}, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok.
