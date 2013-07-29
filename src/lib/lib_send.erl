-module(lib_send).

-compile(export_all).

-include("lib.hrl").

frame(Bin) ->
	Size = size(Bin),
	if 
		Size < 126   ->
			<<1:1, 0:3, 1:4, 0:1, (size(Bin)):7, Bin/binary>>;
		Size < 65536 ->
			<<1:1, 0:3, 1:4, 0:1, 126:7, (size(Bin)):16, Bin/binary>>;
		true         ->
			<<1:1, 0:3, 1:4, 0:1, 127:7, (size(Bin)):64, Bin/binary>>
	end.

one(Socket, Bin) ->
	Frame = frame(Bin),
	gen_tcp:send(Socket, Frame).

all(Bin) ->
	Frame = frame(Bin),
	AllPs = ets:tab2list(?ETS_CLIENT),
	send_raw_to_client(AllPs, Frame).

send_raw_to_client([], _) -> ok;
send_raw_to_client([H|T], Frame) ->
	gen_tcp:send(H#ets_client.socket, Frame),
	send_raw_to_client(T, Frame).

proc(Socket) ->
    receive
        {send, Bin} ->
            gen_tcp:send(Socket, Bin),
            proc(Socket);

        _ ->
            proc(Socket)
    end.