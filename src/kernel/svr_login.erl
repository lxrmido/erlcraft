-module(svr_login).
-behaviour(gen_server).
-export([start_link/0, reg_gid/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-record(state, {gid}).
reg_gid() ->
	gen_server:call(?MODULE, reg_gid).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{gid = 1}}.

handle_call(reg_gid, _, State) ->
	NewState = State#state{
		gid = State#state.gid + 1
	},
	{reply, {ok, State#state.gid}, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.