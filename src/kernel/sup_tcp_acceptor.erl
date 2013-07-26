-module(sup_tcp_acceptor).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{svr_tcp_acceptor, {svr_tcp_acceptor, start_link, []},
            transient, brutal_kill, worker, [svr_tcp_acceptor]}]}}.