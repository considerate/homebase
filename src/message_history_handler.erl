-module (message_history_handler).
-export ([init/2]).
-define(MAX_MESSAGES,30).

init(Req, Opts) ->
	case auth_ball:authenticate(Req) of
		{ok, _Tokendata} ->
			Thread = cowboy_req:binding(threadid, Req),
			QS = cowboy_req:parse_qs(Req),
			After = proplists:get_value(<<"after">>,QS),
			Before = proplists:get_value(<<"before">>,QS),
			
			StartKey = case After of
				undefined -> 
					{"startkey",[Thread]};
				_After ->
					{"startkey",[Thread, After]}
			end,
			
			{QueryParams,QueryOpts} = case Before of
				undefined ->
					EndKey = {"endkey",[Thread,{[]}]},
					{[EndKey],[]};
				_Before -> 
					EndKey = {"endkey",[Thread,Before]},
					Limit = {"limit",?MAX_MESSAGES},
					Decending = {"descending",true},
					{[StartKey,EndKey,Limit,Decending],[reverse]}
			end,

			JSONData = db_utils:query("/_design/Messages/_view/message_history",QueryParams,{opts,QueryOpts}),
			Rows = json_utils:get_field(JSONData,<<"rows">>),
			
			{[FirstMessage|_]} = Rows,
			LastMessage = list:last(Rows),
					
			FirstMessageId = json_utils:get_field(<<"_id">>,FirstMessage),
			LastMessageId = json_utils:get_field(<<"_id">>,LastMessage),			
			
			BaseLink = "/threads/" ++ Thread ++ "/messages",
			GotMaxMessages = length(json_utils:get_field(JSONData,<<"rows">>)) == ?MAX_MESSAGES,
			
			Links = 
				if GotMaxMessages ->
					case {After,Before} of
						{undefined,undefined} -> 
							none;
						{After,undefined} ->
							BeforeLink = web_utils:create_query_url(BaseLink,[{"before",FirstMessageId}]),
							{[{<<"before">>,BeforeLink}]};
						{_,Before} ->
							BeforeLink = web_utils:create_query_url(BaseLink,[{"before",FirstMessageId}]),
							AfterLink = web_utils:create_query_url(BaseLink,[{"after",LastMessageId}]),
							{[{<<"after">>,AfterLink},{<<"before">>,BeforeLink}]}				
					end;
				true ->
					BeforeLink = web_utils:create_query_url(BaseLink,[{"before",FirstMessageId}]),
					AfterLink = web_utils:create_query_url(BaseLink,[{"after",LastMessageId}]),
					{[{<<"after">>,AfterLink},{<<"before">>,BeforeLink}]}	
				end,
			
			JSON = case Links of
				none ->
					JSONData;
				_ ->
					json_utils:add_fields(JSONData,Links)
			end,
			
			web_utils:respond_success(Req,JSON,Opts);
		Error ->
			web_utils:respond_resource_not_found(Req,Error)
	end.

% links suppplied in different situations
% {undefined,undefined} -> na
% {after,undefined} -> before
% {undefined,before} -> before, after
% {after,before} -> before, after
