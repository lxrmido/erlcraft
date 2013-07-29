-module(pp_test).
-compile(export_all).
-include("lib.hrl").

handle("test", Client, [Int, String]) ->
	?T("Int", Int),
	?T("String", String).