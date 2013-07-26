-module(sup_root).
-behaviour(supervisor).
-include("lib.hrl").
-export([start_link/0, start_child/1, start_child/2, init/1]).

start_link() ->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).

start_child(Mod) ->
    start_child(Mod, []).

start_child(Mod, Args) ->
    {ok, _} = supervisor:start_child(?MODULE,
                                     {Mod, {Mod, start_link, Args},
                                      transient, 100, worker, [Mod]}),
    ok.

init([]) ->
    {ok, {   
            {one_for_one, 3, 10},   
            []         
        }
    }. 