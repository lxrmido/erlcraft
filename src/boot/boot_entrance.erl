-module(boot_entrance).
-compile(export_all).
-include("lib.hrl").
start(normal, []) ->
	{ok, SupPid} = sup_root:start_link(),
    ok = init_ets(),
	{ok,_} = supervisor:start_child(
        sup_root,
        {sup_tcp_client,
            {sup_tcp_client, start_link,[]},
            transient, infinity, supervisor, [sup_tcp_client]}
    ),
	{ok,_} = supervisor:start_child(
        sup_root,
        {sup_tcp,
            {sup_tcp, start_link,[10086]},
            transient, infinity, supervisor, [sup_tcp]}
    ),
    {ok, SupPid}.

init_ets() ->
    ets:new(ets_vars,   [named_table, public, set]),
    ets:new(ets_client, [named_table, public, set]),
    ok.