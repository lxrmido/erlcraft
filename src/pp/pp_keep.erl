-module(pp_keep).
-export([handle/3]).
-include("lib.hrl").

handle("live", Client, []) ->
	{client, Client}.