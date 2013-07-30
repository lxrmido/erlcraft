-module(en).
-compile(export_all).
-define(SERVER_APPS, [sasl, server]).

s()->
    try
        ok = start_applications(?SERVER_APPS)
    after
        timer:sleep(100)
    end.

q() ->
    stop_applications(?SERVER_APPS).

%%############辅助调用函数##############
manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    Iterate(fun (App, Acc) ->
                case Do(App) of
                    ok -> [App | Acc];%合拢
                    {error, {SkipError, _}} -> Acc;
                    {error, Reason} ->
                        lists:foreach(Undo, Acc),
                        throw({error, {ErrorTag, App, Reason}})
                end
        end, [], Apps),
    ok.

start_applications(Apps) ->
    manage_applications(fun lists:foldl/3,
        fun application:start/1,
        fun application:stop/1,
        already_started,
        cannot_start_application,
        Apps).

stop_applications(Apps) ->
    manage_applications(fun lists:foldr/3,
        fun application:stop/1,
        fun application:start/1,
        not_started,
        cannot_stop_application,
        Apps).
