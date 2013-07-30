-module(lang).
-compile(export_all).
-include("lib.hrl").

addslashes(Bin) ->
	addslashes(Bin, <<>>).
addslashes(<<>>, B) -> B;
addslashes(<<"\\\\", R/binary>>, B) ->
	addslashes(R, <<B/binary, "\\\\">>);
addslashes(<<"\\\'", R/binary>>, B) ->
	addslashes(R, <<B/binary, "\\\'">>);
addslashes(<<"\\\"", R/binary>>, B) ->
	addslashes(R, <<B/binary, "\\\"">>);
addslashes(<<"\\u", R/binary>>, B) ->
	addslashes(R, <<B/binary, "\\u">>);
addslashes(<<"\\n", R/binary>>, B) ->
	addslashes(R, <<B/binary, "<br />">>);
addslashes(<<"\\t", R/binary>>, B) ->
	addslashes(R, <<B/binary, "&emsp;">>);
addslashes(<<"\\/", R/binary>>, B) ->
	addslashes(R, <<B/binary, "\\/">>);
addslashes(<<"\\", R/binary>>, B) ->
	addslashes(R, <<B/binary, "\\\\">>);
addslashes(<<"\'", R/binary>>, B) ->
	addslashes(R, <<B/binary, "\\\'">>);
addslashes(<<"\"", R/binary>>, B) ->
	addslashes(R, <<B/binary, "\\\"">>);
addslashes(<<X:8, R/binary>>, B) ->
	addslashes(R, <<B/binary, X:8>>).