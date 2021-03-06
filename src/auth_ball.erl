-module(auth_ball).
-export([authenticate/1, rest_auth/2, secret/0, forbidden_from_creator/2, forbidden_from_thread/2,forbidden_from_user/2]).

secret() ->
    {ok, Secret} = application:get_env(homebase,jwt_secret),
    Secret.

authenticate(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Bearer ", Token/binary>> ->
            case catch ejwt:parse_jwt(Token, secret()) of
                {Data} -> {ok, Data};
                Error -> {error, Error}
            end;
        <<_>> ->
            {error, not_bearer_authorization};
        undefined ->
            {error, no_authorization_header}

    end.

rest_auth(Req, State) ->
    case {cowboy_req:method(Req), authenticate(Req)} of
        {<<"OPTIONS">>, _} ->
            {true, Req, State};
        {_, {ok, Data}} ->
            Uid = proplists:get_value(<<"id">>,Data),
            {true, Req, [{user,Uid}|State]};
        {_, {error, Error}} ->
            lager:debug("authentication failed: ~p", [Error]),
            {{false, <<"Authorization">>}, Req, State}
    end.

%Forbidden if the requested thread exists and the requesting user is not a member of it.
forbidden_from_thread(Req,State) ->
    Method = cowboy_req:method(Req),
    NewState = append_doc_to_state(threadid,Req,State),
    Document = proplists:get_value(document,NewState),
    Forbidden = case {Method, Document} of
        {<<"OPTIONS">>, _} ->
            false;
        {_, {Thread}} -> 
            RequestingUser = proplists:get_value(user,State),
            Users = proplists:get_value(<<"users">>,Thread),
            not lists:member(RequestingUser,Users);
        {_, none} ->
            false
    end,
    {Forbidden,Req,NewState}.

%Forbidden if requested thread exists and the requesting user is not the creator of it.
forbidden_from_creator(Req,State) ->
    Method = cowboy_req:method(Req),
    NewState = append_doc_to_state(threadid,Req,State),
    Document = proplists:get_value(document,NewState),
    Forbidden = case {Method, Document} of
        {<<"OPTIONS">>, _} ->
            false;
        {_, {Thread}} -> 
            RequestingUser = proplists:get_value(user,State),
            Creator = proplists:get_value(<<"creator">>,Thread),
            Creator =/= RequestingUser;
        {_, none} ->
            false
    end,
    {Forbidden,Req,NewState}.

%Forbidden if user specified in url
forbidden_from_user(Req,State) ->
    Method = cowboy_req:method(Req),
    URLUserID = web_utils:get_user_id(Req,State),
    ActualUserID = proplists:get_value(user,State),
    Forbidden = Method =/= <<"OPTIONS">>
        andalso ActualUserID =/= URLUserID,
    {Forbidden,Req,State}.

%Appends a document from couch with the id specifyed by the given url binding.
append_doc_to_state(Binding,Req,State) ->
    DocID = cowboy_req:binding(Binding,Req),
    Doc = case '3rd-base_db_utils':fetch(DocID) of
        {ok,Obj} ->
            Obj;
        {error,_Err} ->
            none
    end,
    [{document,Doc}|State].
	

