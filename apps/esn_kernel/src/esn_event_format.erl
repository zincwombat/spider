-module(esn_event_format).
-vsn('$Revision: 1.2 $').
-author('$Author: thobbins $').
-date('$Date: 2008/06/18 01:33:20 $').
-rcsid('@(#) $Id: esn_event_format.erl,v 1.2 2008/06/18 01:33:20 thobbins Exp $\n').


-export([	format/2,
		formatDate/1,
		formatEvent/2]).
	
format(Type,Detail)->
	format({date(),time()},node(),Type,Detail).

format(Calendar,Node,Type,Detail)->
	C=formatDate(Calendar),
	io_lib:format("~s ~s ~s ~p~n",[C,prNode(Node),Type,Detail]).

formatEvent(Event,esn_progress)->
	{Calendar,Node,{_T,_Pid,{_OPid,esn_report,{esn_progress,Detail}}}}=Event,
	format(Calendar,Node,"*PRG*",Detail);

formatEvent(Event,esn_trace)->
	{Calendar,Node,{_T,_Pid,{_OPid,esn_report,{esn_trace,Detail}}}}=Event,
	format(Calendar,Node,"<TRA>",Detail);

formatEvent(Event,esn_warn)->
	{Calendar,Node,{_T,_Pid,{_OPid,esn_report,{esn_warn,Detail}}}}=Event,
	format(Calendar,Node,"<WARN>",Detail);

formatEvent(Event,esn_info)->
	{Calendar,Node,{_T,_Pid,{_OPid,esn_report,{esn_info,Detail}}}}=Event,
	format(Calendar,Node,"<INFO>",Detail);

formatEvent(Event,esn_debug)->
	{Calendar,Node,{_T,_Pid,{_OPid,esn_report,{esn_debug,Detail}}}}=Event,
	format(Calendar,Node,"<DBUG>",Detail);

formatEvent(Event,esn_critical)->
	{Calendar,Node,{_T,_Pid,{_OPid,esn_report,{esn_critical,Detail}}}}=Event,
	format(Calendar,Node,"<CRIT>",Detail);

formatEvent(Event,erlang_emulator_error)->
	{Calendar,Node,{emulator,_GL,Detail}}=Event,
	format(Calendar,Node,"<EMU>",Detail);

formatEvent(Event,erlang_error)->
	{Calendar,Node,{error,_Pid,{_OPid,ErrorFormat,ErrorInfo}}}=Event,
	ErrorString=
	case catch io_lib:format(ErrorFormat,ErrorInfo) of
	{'EXIT',_Reason}->
		{{format,ErrorFormat},{values,ErrorInfo}};
	String->
%%		{{format,ErrorFormat},{values,ErrorInfo}}
		%% SHOULD DO SOMETHING HERE, SOME OUTPUTS CAN BE UGLY
		lists:flatten(String)
	end,
	format(Calendar,Node,"<ERR>",ErrorString);
	
formatEvent(Event,erlang_info)->
	{Calendar,Node,{_T,_Pid,{_OPid,Type,Detail}}}=Event,
	format(Calendar,Node,"[INF]",{Type,Detail});

formatEvent(Event,sasl_crash)->
	{Calendar,Node,{_T,_Pid,{_OPid,_Type,Detail}}}=Event,
	format(Calendar,Node,"[CRA]",Detail);

formatEvent(Event,sasl_progress)->
	{Calendar,Node,{_T,_Pid,{_OPid,_Type,Detail}}}=Event,
	format(Calendar,Node,"[PRG]",Detail);

formatEvent(Event,sasl_supervisor)->
	{Calendar,Node,{_T,_Pid,{_OPid,_Type,Detail}}}=Event,
	format(Calendar,Node,"[SUP]",Detail);

formatEvent({Calendar,Node,{info_msg,_Pid,{_OPid,Fmt,Detail}}},_)->
	format(Calendar,Node,"[INF]",lists:flatten(io_lib:format(Fmt,Detail)));

formatEvent(Event,OtherType)->
	%% DO SOMETHING HERE !!!!!!!
	io_lib:format("====UNKNOWN EVENT TYPE====~n~p~n~p~n",[Event,OtherType]).


zeropad(Num,Size)->
	if length(Num) < Size->
		zeropad("0"++Num,Size);
	true->
		Num
	end.

formatDate({{Y,Mo,D},{H,Mi,S}})->
	Day=zeropad(integer_to_list(D),2),
	io_lib:format(	"~s-~s-~p ~s:~s:~s",
			[Day,month(Mo),Y,t(H),t(Mi),t(S)]).

t(X) when is_integer(X)->
	t1(integer_to_list(X));

t(_)->
	"".

t1([X])->[$0,X];
t1(X)->X.

padString(S,PadChar,Len)->
	if length(S) < Len->
		padString(S++PadChar,PadChar,Len);
	true->
		S
	end.

prNode(Node)->
	N=atom_to_list(Node),
	[N1,Host]=string:tokens(N,"@"),
	[H1|_Rest]=string:tokens(Host,"."),
	X=
	if N1=="hostmgr"->
		"HM@"++H1;
	true->
		N1
	end,
	padString(X," ",12).

month(1) ->"Jan";
month(2) ->"Feb";
month(3) ->"Mar";
month(4) ->"Apr";
month(5) ->"May";
month(6) ->"Jun";
month(7) ->"Jul";
month(8) ->"Aug";
month(9) ->"Sep";
month(10)->"Oct";
month(11)->"Nov";
month(12)->"Dec".


