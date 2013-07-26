-module(sup_tcp).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(Port) ->
    supervisor:start_link(?MODULE, {10, Port}).

init({AcceptorCount, Port}) ->
    {ok,
        {{one_for_all, 10, 10},
            [
                {
                    sup_tcp_acceptor,
                    {sup_tcp_acceptor, start_link, []},
                    transient,
                    infinity,
                    supervisor,
                    [sup_tcp_acceptor]
                },
                {
                    svr_tcp_listener,
                    {svr_tcp_listener, start_link, [AcceptorCount, Port]},
                    transient,
                    100,
                    worker,
                    [svr_tcp_listener]
                }
            ]
        }
    }.
