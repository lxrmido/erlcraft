-define(T(X), io:format("~n====~p====~nL:~p~n~s~n", [?MODULE, ?LINE, X])).
-define(T(X, Y), io:format("~n====~p====~nL:~p~n~s:~p~n", [?MODULE, ?LINE, X, Y])).
-define(I(X), io:format("[INFO][~p][~s]~n", [?MODULE, X])).
-define(I(X, Y), io:format("[INFO][~p][~s]:~p~n", [?MODULE, X, Y])).

-record(ets_client, {
	pid,
	uid = 0,
	username = <<>>,
	socket
}).