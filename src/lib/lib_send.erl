-module(lib_send).

-compile(export_all).

one(Socket, Bin) ->
	Frame = <<1:1, 0:3, 1:4, 0:1, (size(Bin)):7, Bin/binary>>,
	gen_tcp:send(Socket, Frame).