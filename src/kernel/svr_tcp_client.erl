-module(svr_tcp_client).
-compile(export_all).
-include("lib.hrl").

start_link() ->
	{ok, proc_lib:spawn_link(?MODULE, init, [])}.

init() ->
	receive
        {go, Socket} ->
            parse_packet(Socket, #client{socket = Socket})
    end.

parse_packet(Socket, Client) ->
	Ref = async_recv(Socket, 3, 100000),
	receive 
		{inet_async, Socket, Ref, {ok, <<"GET">>}} ->
			parse_packet_header(Socket, Client);
		Error ->
			?T("ERROR", Error),
			gen_tcp:close(Socket),
			exit({unexpected_message, "normal_close"})
	end.

parse_packet_header(Socket, Client) ->
	Ref = async_recv(Socket, 1, 100),
	receive 
		{inet_async, Socket, Ref, {ok, <<"\r">>}} ->
			parse_packet_header(Socket, Client);
		{inet_async, Socket, Ref, {ok, <<"\n">>}} ->
			Client1 = Client#client{
				buffer = <<>>,
				lines = [Client#client.buffer | Client#client.lines]
			},
			parse_packet_header(Socket, Client1);
		{inet_async, Socket, Ref, {ok, Byte}} ->
			Buffer = Client#client.buffer,
			Client2 = Client#client{
				buffer = <<Buffer/binary, Byte/binary>>
			},
			parse_packet_header(Socket, Client2);
		_ ->
			Client3 = decode_ws_header(Client),
			gen_tcp:send(Socket, 
				calc_reply(
					Client3#client.host,
					Client3#client.accept
				)
			),
			PSend = spawn_link(fun() -> lib_send:proc(Socket) end),
			EC = #ets_client{
				p_recv = self(),
				p_send = PSend,
				socket = Socket
			},
			GId = user:guest(EC),
			Client4 = Client3#client{
				gid = GId
			},
			parse_packet_msg(Socket, Client4)
	end.

parse_packet_msg(Socket, Client) ->
	Ref = async_recv(Socket, 2, 1000000),
	receive 
		{inet_async, Socket, Ref, {ok, <<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, Len:7>>}} ->
			parse_packet_body(Socket, Client, Len);
		Error ->
			?T("ERROR", Error),
			close(Client),
			exit({unexpected_message, "normal_close"})
	end.

parse_packet_body(Socket, Client, Len) ->
	Ref = async_recv(Socket, Len + 4, 1000000),
	receive 
		{inet_async, Socket, Ref, {ok, Data}} ->
			<<Masking:4/binary, Payload/binary>> = Data,
			Line = unmask(Payload, Masking),
			% ?T("Line", Line),
			<<M1:4/binary, M2:4/binary, MsgBody/binary>> = Line,
			case routing(M1, M2, MsgBody, Client) of 
				{client, Client2} ->
					parse_packet_msg(Socket, Client2);
				_ ->
					parse_packet_msg(Socket, Client)
			end;
		Error ->
			?T("ERROR", Error),
			close(Client),
			exit({unexpected_message, "normal_close"})
	end.

routing(M1, M2, MsgBody, Client) ->
	MDB = <<"pp_", M1/binary>>,
	MDS = binary_to_list(MDB),
	MDA = list_to_atom(MDS),
	ARG = pack:unpack(MsgBody),
	MDA:handle(binary_to_list(M2), Client, ARG).


async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case prim_inet:async_recv(Sock, Length, Timeout) of
        {error, Reason} -> throw({Reason});
        {ok, Res}       -> Res;
        Res             -> Res
    end.

decode_ws_header(Client) ->
	case Client#client.lines of 
		[]    -> Client;
		[H|T] ->
			case H of 
				<<"Host: ", Host/binary>> ->
					decode_ws_header(
						Client#client{
							host  = Host,
							lines = T
						}
					);
				<<"Sec-WebSocket-Key: ", Key/binary>> ->
					Accept = calc_ws_key(Key),
					decode_ws_header(
						Client#client{
							key  = Key,
							lines = T,
							accept = Accept
						}
					);
				_ ->
					Headers = Client#client.headers,
					decode_ws_header(
						Client#client{
							lines = T,
							headers = [H | Headers]
						}
					)
			end
	end.

calc_ws_key(RawKey) ->
	MagicKey = <<RawKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>,
	Bin = sha1:encode_bin(MagicKey),
	list_to_binary(base64:encode_to_string(Bin)).

calc_reply(Host, Accept) ->
	<<
		"HTTP/1.1 101 Switching Protocols\r\n",
		"Upgrade: websocket\r\n",
		"Connection: Upgrade\r\n",
		"Sec-WebSocket-Accept: ", Accept/binary, "\r\n",
		"Sec-WebSocket-Origin: null\r\n", 
		"Sec-WebSocket-Location: ws://", Host/binary,"\r\n",
		"\r\n"
	>>.

close(Client) ->
	gen_tcp:close(Client#client.socket),
	ets:delete(?ETS_CLIENT, Client#client.gid).

send(Socket, Bin) ->
	Frame = <<1:1, 0:3, 1:4, 0:1, (size(Bin)):7, Bin/binary>>,
	gen_tcp:send(Socket, Frame).

unmask(Payload, Masking) ->
    unmask(Payload, Masking, <<>>).
 
unmask(Payload, Masking = <<MA:8, MB:8, MC:8, MD:8>>, Acc) ->
    case size(Payload) of
        0 -> Acc;
        1 ->
            <<A:8>> = Payload,
            <<Acc/binary, (MA bxor A)>>;
        2 ->
            <<A:8, B:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B)>>;
        3 ->
            <<A:8, B:8, C:8>> = Payload,
            <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
        _Other ->
            <<A:8, B:8, C:8, D:8, Rest/binary>> = Payload,
            Acc1 = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
            unmask(Rest, Masking, Acc1)
    end.
 