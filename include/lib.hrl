-define(T(X), io:format("~n====~p====~nL:~p~n~s~n", [?MODULE, ?LINE, X])).
-define(T(X, Y), io:format("~n====~p====~nL:~p~n~s:~p~n", [?MODULE, ?LINE, X, Y])).
-define(I(X), io:format("[INFO][~p][~s]~n", [?MODULE, X])).
-define(I(X, Y), io:format("[INFO][~p][~s]:~p~n", [?MODULE, X, Y])).

-record(ets_client, {
	id,
	p_recv,
	p_send,
	socket
}).

-record(client, {
	gid = 0,
	buffer = <<>>,
	lines = [],
	host = <<>>,
	key = <<>>,
	headers = [],
	accept = <<>>,
	uid = 0,
	username = <<>>,
	socket
}).

-define(ETS_VARS  , ets_vars).
-define(ETS_CLIENT, ets_client).
-define(ETS_ELS, ets_els).