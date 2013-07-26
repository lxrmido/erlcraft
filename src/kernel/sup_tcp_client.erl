-module(sup_tcp_client).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 10, 10},
          [{svr_tcp_client, {svr_tcp_client,start_link,[]},
            temporary, brutal_kill, worker, [svr_tcp_client]}]}}.

