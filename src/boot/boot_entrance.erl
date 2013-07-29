-module(boot_entrance).
-compile(export_all).
-include("lib.hrl").
start(normal, []) ->
    {ok, JaHost}   = application:get_env(ja_host),
    {ok, JaPort}   = application:get_env(ja_port),
    {ok, JaAction} = application:get_env(ja_action),
    {ok, SupPid}   = sup_root:start_link(),
    ok             = init_ets(),
	{ok, _} = supervisor:start_child(
        sup_root,
        {sup_tcp_client,
            {sup_tcp_client, start_link,[]},
            transient, infinity, supervisor, [sup_tcp_client]}
    ),
	{ok, _} = supervisor:start_child(
        sup_root,
        {sup_tcp,
            {sup_tcp, start_link,[10086]},
            transient, infinity, supervisor, [sup_tcp]}
    ),
    {ok, _} = supervisor:start_child(
        sup_root,
        {svr_login,
            {svr_login, start_link,[]},
            transient, infinity, supervisor, [svr_login]}
    ),
    {ok, JaPid} = supervisor:start_child(
        sup_root,
        {mod_ja,
            {mod_ja, start_link,[]},
            transient, infinity, worker, [mod_ja]}
    ),
    JaPid ! {connect, JaHost, JaPort, JaAction},
    {ok, SupPid}.

init_ets() ->
    ets:new(?ETS_VARS,   [named_table, public, set]),
    ets:new(?ETS_CLIENT, [named_table, public, set, {keypos, #ets_client.id}]),
    ok.