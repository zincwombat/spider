-module(text).
-export([compress/1,
	 wstrcompress/1,
	 xmlEntityEncode/1,
	 filter/1,
	 wstranslate/1]).

-export([i_wstrcompress/1,
	i_wstranslate/1,
	i_xmlEntityEncode/1,
	i_compress/1]).

%% general text filtering and processing module

%% compress/1 removes all adjacent WS chars
%% wstranslate/1 translates all WHITESPACE chars to SPACE
%% wstrcompress/1 combines compress and wstranslate
%% xmlEntityEncode/1 converts unsafe XML chars to their equivalent entity declarations

-include("debug.hrl").
-include("text.hrl").

%% public API

compress(Ls)->
	op(i_compress,Ls).

wstranslate(Ls)->
	op(i_wstranslate,Ls).

wstrcompress(Ls)->
	op(i_wstrcompress,Ls).

xmlEntityEncode(Ls)->
	op(i_xmlEntityEncode,Ls).


op(OpType,Bin) when is_binary(Bin)->
	%% convert binary to list 
	op(OpType,binary_to_list(Bin));

op(OpType,Ls=[H|Rest]) when is_integer(H)->
	%% this is a string, call opType
	i_op(OpType,Ls);

op(OpType,X=[Ls])->
	op(OpType,Ls);

op(OpType,Ls) when is_list(Ls)->
	%% lists - apply to each member
	lists:map(fun(Z)->op(OpType,Z) end,Ls);

op(OpType,Other)->
	?warn({badarg,{op,OpType},Other}),
	{error,badarg}.


i_op(OpType,Ls)->
	case (catch apply(?MODULE,OpType,[Ls])) of
	E={'EXIT',Reason}->
		{error,E};

	Result->
		Result
	end.

%% operation specifics -- internal API

i_wstrcompress(Ls)->
	ZZ=i_wstranslate(Ls),
	filter(i_compress(ZZ)).

i_wstranslate(Ls)->
	i_translate(Ls,?WHITESPACE,?SPACE).

i_translate(Ls,From,To) when is_list(Ls) ->
	lists:map(fun(Z)->i_translate(Z,From,To) end,Ls);

i_translate(Char,From,To) when is_integer(Char)->
	case lists:member(Char,From) of
	true->
		To;
	_->
		Char
	end.


i_compress(Ls)->
	Z=ii_compress(Ls,[]),
	lists:reverse(Z).

ii_compress([H1],Acc)->
	case isWhiteSpace(H1) of
	true->
		Acc;
	_->
		[H1|Acc]
	end;

ii_compress([H1|Rest],Acc=[H2|RAcc])->
	case p(H1,H2) of
	true->
		ii_compress(Rest,Acc);
	_->
		ii_compress(Rest,[H1|Acc])
	end;

ii_compress([H1|Rest],[])->
	ii_compress(Rest,[H1]);


ii_compress([],Acc)->
	Acc.

i_xmlEntityEncode(Ls)->
	Z=lists:map(fun(Z)->map(Z) end,Ls),
	lists:flatten(Z).

filter(Ls=[H|_]) when is_integer(H)->
	lists:filter(fun(Z)->i_filter(Z) end,Ls);

filter(Ls)->
	Ls.

i_filter(Char) when Char>=0,Char=<8->
	false;

i_filter(Char) when Char>8,Char=<126->
	true;

i_filter(_)->
	false.

p(H1,H2)->
	p(H1,H2,isWhiteSpace(H1),isWhiteSpace(H2)).

p(H1,H2,true,true)->
	true;

p(_,_,_,_)->
	false.

isWhiteSpace(H)->
	%% return true if H is a whitespace character
	lists:member(H,?WHITESPACE).




map($<)->
	"&lt;";

map($>)->
	"&gt;";

map($&)->
	"&amp;";

map($\")->
	"&quot;";

map(Other)->
	Other.
