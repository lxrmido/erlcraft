-module(pp_regs).
-export([handle/3]).
-include("lib.hrl").

handle("ctrl", Client, []) ->
	els:reg(ctrl, Client#client.gid).