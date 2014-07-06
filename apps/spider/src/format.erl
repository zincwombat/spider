-module(format).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([formatList/4]).

-define(COMMA,	",").



%% ============================================================================

csv(List)->
	%% no prefix
	%% no transform fun
	%% just convert List to a CSV format

	formatList(?COMMA,[],List,null).

formatList(Delimiter,Prefix,List,Fun) when is_list(List)->
	%% Prefix the CSV string with Prefix
	%% apply Fun to List
	%% output List in CSV format

	formatList(Delimiter,Prefix,List,Fun,[]);

formatList(_,_,Other,_)->
	?warn({cvsBadArg,Other}),
	{error,{badarg,Other}}.

formatList(Delimiter,Prefix,List,Fun,[]) when is_function(Fun)->
	%% transform List->NewList by applying the function
	%% recurse the function with Fun set to null

	NewList=lists:map(fun(Z)->Fun(Z) end,List),
	formatList(Delimiter,Prefix,NewList,null,[]);

formatList(Delimiter,Prefix,List,Fun,[])->
	%% ENTRY POINT - first entry in list

	delimit(Delimiter,Prefix,i_formatList(Delimiter,List,[])).

%% the list conversion to CSV is done here

i_formatList(Delimiter,[Item],Acc)->
	%% one entry in list

	delimit(Delimiter,Acc,Item);

i_formatList(Delimiter,[Item|Rest],Acc)->
	%% multiple entries in list
	i_formatList(Delimiter,Rest,delimit(Delimiter,Acc,Item));

i_formatList(Delimiter,[],Acc)->
	%% no entry in list, print out list
	Acc.

delimit(D,[],Arg2)->
	Arg2;

delimit(D,Arg1,[])->
	Arg1;

delimit(D,Arg1,Arg2)->
	case (catch Arg1++D++Arg2) of
	{'EXIT',Reason}->
		{error,badarg};
	Other->
		lists:flatten(Other)
	end.

comma(Arg1,Arg2)->
	delimit(",",Arg1,Arg2).

