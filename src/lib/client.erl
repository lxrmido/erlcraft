-module(client).
-compile(export_all).
-include("lib.hrl").

start() ->
    start("mc.bilicraft.com", 25572).

start(Host, Port) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}]) of
        {ok, Sock} ->
            Pid = spawn(fun process_hand/0),
            gen_tcp:controlling_process(Sock, Pid),
            Pid ! {bind_sock, Sock},
            gen_tcp:send(Sock, <<"/api/subscribe?source=console&key=dced10a4a6be5ccc82a21086fac34b9279054edb708b9da31cdedb710d6ee918\n">>),
            ?T("OPEN", {Sock}),
            handle_cmd(Pid),
            {ok, Pid, Sock};
        Else ->
            Else
    end.

process_hand() ->
    process_recv(null).

process_recv(Socket) ->
    receive
        {bind_sock, Sock} ->
            process_recv(Sock);
        {tcp, Socket, Bin} ->
            handle_bin(Bin),
            process_recv(Socket);
        {tcp_closed, Reason} ->
        	?T("recv_closed", Reason);
        {send, Msg} ->
        	gen_tcp:send(Socket, Msg),
        	process_recv(Socket);
       	{exit, Reason} ->
       		?T("recv_exit", Reason);
        Other ->
        	?T("recv_error", Other)
    end.
handle_bin(Bin) ->
	Str = erlang:binary_to_list(Bin),
	?T("Bin", Str). 

handle_cmd(Pid) ->
    Cmd = list_to_binary(io:get_line(">")),
    try
        case Cmd of
            <<"s\s", Content/binary>> ->
                Pid ! {send, Content},
                handle_cmd(Pid);
            <<"q", _/binary>> ->
            	Pid ! {exit, normal};
            <<"\n">> -> 
                handle_cmd(Pid);
            <<"\r">> ->
            	handle_cmd(Pid)
        end
    catch
        _Class:_Err ->
            %?T({Err, Cmd}),
            io:format("~n?>"),
            handle_cmd(Pid)
    end.