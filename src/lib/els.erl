-module(els).
-compile(export_all).
-include("lib.hrl").

reg(Name, GId) ->
	case ets:info(Name) of 
		undefined ->
			ets:new(Name, [named_table, public, set]),
			ets:insert(Name, {GId}),
			ok;
		_ ->
			ets:insert(Name, {GId}),
			ok 
	end.

del(Name, GId) ->
	case ets:info(Name) of 
		undefined -> ok;
		_ ->
			ets:delete(Name, GId),
			ok 
	end.

lsa(Name) ->
	case ets:info(Name) of 
		undefined -> [];
		_         -> ets:tab2list(Name)
	end.