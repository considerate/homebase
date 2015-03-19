-module(json_utils).
-export([add_fields/2,get_field/2]).

add_fields(Fields,{Obj}) ->
	{lists:append(Obj,Fields)}.
	
get_field(Field_name,{Obj}) ->
	proplists:get_value(Field_name,Obj).
