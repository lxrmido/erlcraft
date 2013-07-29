-module(user).
-include("lib.hrl").
-compile(export_all).

guest(Client) ->
	{ok, GId} = svr_login:reg_gid(),
	EtsClient = Client#ets_client{
		id     = GId
	},
	ets:insert(?ETS_CLIENT, EtsClient),
	GId.

