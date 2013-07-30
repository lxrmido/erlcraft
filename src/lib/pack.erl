-module(pack).
-compile(export_all).
-include("lib.hrl").

%
% I : Integer  |  I1234;
% S : String   |  S5;hello
%

unpack(Bin) -> unpack(Bin, []).
unpack(<<>>, List) -> lists:reverse(List);
unpack(<<$I, Rest/binary>>, List) ->
	{Item, Rest2} = unpack_integer(Rest),
	unpack(Rest2, [Item | List]);
unpack(<<$S, Rest/binary>>, List) ->
	{Item, Rest2} = unpack_string(Rest),
	unpack(Rest2, [Item | List]);
unpack(_, List) -> lists:reverse(List).

unpack_integer(Bin) ->
	unpack_integer(Bin, <<>>).
unpack_integer(<<>>, Cur) -> 
	Int = list_to_integer(binary_to_list(Cur)),
	{Int, <<>>};
unpack_integer(<<$;, Rest/binary>>, Cur) -> 
	Int = list_to_integer(binary_to_list(Cur)),
	{Int, Rest};
unpack_integer(<<B:8, Rest/binary>>, Cur) ->
	unpack_integer(Rest, <<Cur/binary, B:8>>).

unpack_string(Bin) -> 
	{Length, Rest} = unpack_integer(Bin),
	<<Str:Length/binary, Rest2/binary>> = Rest,
	{binary_to_list(Str), Rest2}. 

pack(List) -> pack(List, <<>>).
pack([], Bin) -> <<$[, Bin/binary, $]>>;
pack([H|T], <<>>) when is_integer(H) ->
	I1 = integer_to_list(H),
	I2 = list_to_binary(I1),
	pack(T, I2);
pack([H|T], Bin) when is_integer(H) ->
	I1 = integer_to_list(H),
	I2 = list_to_binary(I1),
	B1 = <<Bin/binary, $,, I2/binary>>,
	pack(T, B1);
pack([H|T], <<>>) when is_list(H) ->
	S1 = list_to_binary(H),
	B1 = <<$", S1/binary, $">>,
	pack(T, B1);
pack([H|T], Bin) when is_list(H) -> 
	S1 = list_to_binary(H),
	B1 = <<Bin/binary, $,, $", S1/binary, $">>,
	pack(T, B1);
pack([H|T], <<>>) when is_binary(H) ->
	pack(T, H);
pack([H|T], Bin) when is_binary(H) ->
	B1 = <<Bin/binary, $,, $", H/binary, $">>,
	pack(T, B1);
pack([_|T], <<>>) ->
	B1 = <<"null">>,
	pack(T, B1);
pack([_|T], Bin) ->
	B1 = <<Bin/binary, ",null">>,
	pack(T, B1).