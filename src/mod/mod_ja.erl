-module(mod_ja).

-include("lib.hrl").

-compile(export_all).

-record(ja, {
	host,
	port,
	action
}).

start_link() ->
	{ok, proc_lib:spawn_link(?MODULE, init, [])}.

init() ->
	receive
        {connect, Host, Port, Action} ->
        	Ja = #ja{
        		host   = Host,
        		port   = Port,
        		action = Action
        	},
        	put(ja, Ja),
            try_connect(Ja);
        {connect, Ja} ->
        	put(ja, Ja),
            try_connect(Ja)
    end.

try_connect() ->
	Ja = get(ja),
	try_connect(Ja).
try_connect(Ja) ->
	?I("CONNECT_TRY"),
	case gen_tcp:connect(
		Ja#ja.host, 
		Ja#ja.port, 
		[binary, {packet, 0}]
	) of 
		{ok, Socket} ->
			?I("CONNECT_OK", Socket),
			gen_tcp:send(
				Socket, 
				Ja#ja.action
			),
			parse(Socket);
		Else ->
			?I("CONNECT_FAILED", Else),
			erlang:send_after(3000, self(), reconnect),
			parse(null)
	end.

parse(Socket) ->
	receive 
		reconnect ->
            try_connect();
        {reset, Host, Port, Action} ->
        	Ja = #ja{
        		host   = Host,
        		port   = Port,
        		action = Action
        	},
        	put(ja, Ja),
            try_connect(Ja);
        {tcp, Socket, Bin} ->
            handle_bin(Bin),
            parse(Socket);
        {tcp_closed, Reason} ->
        	?T("recv_closed", Reason),
        	try_connect();
        {send, Msg} ->
        	gen_tcp:send(Socket, Msg),
        	parse(Socket);
       	{exit, Reason} ->
       		?T("recv_exit", Reason),
       		exit({recv_exit, Reason});
        Other ->
        	?T("recv_error", Other),
        	parse(Socket)
    end.

handle_bin(Bin) ->
    JS  = js_splits(Bin),
    F = fun(X) ->
        ?T("JA", X),
        lib_send:all(X)
    end,
    lists:foreach(F, JS).

js_splits(O) ->
    lists:reverse(js_splits(O, [])).
js_splits(<<>>, L) -> L;
js_splits(B, L) ->
    case js_split(B) of 
        {J, R} -> js_splits(R, [J|L]);
        fail   -> L
    end.
js_split(<<>>) -> fail;
js_split(<<${, _/binary>> = O) ->
    js_split(O, <<>>, 0, 0);
js_split(<<_:8, O/binary>>) ->
    js_split(O).
js_split(R, N, LC, RC) when LC =:= RC andalso LC =/= 0 -> {<<${, N/binary, $}>>, R};
js_split(<<>>, N, _, _) -> {N, <<>>};
js_split(<<${, R/binary>>, N, LC, RC) ->
    js_split(R, N, LC + 1, RC);
js_split(<<$}, R/binary>>, N, LC, RC) ->
    js_split(R, N, LC, RC + 1);
js_split(<<X:8, R/binary>>, N, LC, RC) ->
    js_split(R, <<N/binary, X:8>>, LC, RC).