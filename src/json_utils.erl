-module(json_utils).
-export([add_fields/2,get_field/2]).

add_fields({Obj},Fields) ->
	{lists:append(Obj,Fields)}.
	
get_field({Obj},Field_name) ->
	proptylists:get_value(Field_name).
