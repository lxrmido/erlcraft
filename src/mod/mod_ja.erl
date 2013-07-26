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
	Str = erlang:binary_to_list(Bin),
	?T(Str). 