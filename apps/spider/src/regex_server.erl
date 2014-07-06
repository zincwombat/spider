-module(regex_server).
-behaviour(gen_server).
-define(TRACE_LEVEL,?TRACE_WARN).
-include_lib("esn_kernel/include/debug.hrl").

%% This module implements a regular expression server


-export([       init/1,
                handle_call/3,
                handle_cast/2,
                handle_info/2,
		code_change/3,
                terminate/2
                ]).

-export([       
		start/0,
                start/1,
		compile/2,
		compile/3,
		match/2,
		exec/2,			%% extract first match
		extractMatches/3,	%% extract matches from the entire body
		extractMatchPos/3,	%% extract the match positions
                status/1,
                stop/1]).

-define(START_OPTIONS,          []).
-define(SERVERNAME,             ?MODULE).

-include("regex.hrl").

-sysloglevel(?TRACE_WARN).

-record(state,{tag,re,port,parsefun}).

parse(Tag=email,{set,R=[Mail|_]})->
	?dbug({parse,{Tag,R}}),
	Mail;

parse(Tag=href,{set,R=[_Match,URL|_Rest]})->
	?dbug({parse,{Tag,R}}),
	URL;

parse(Tag=url,{set,R=[URL|_Rest]})->
	?dbug({parse,{Tag,R}}),
	URL;

parse(Tag=domain,{set,R=[Domain|_Rest]})->
	?dbug({parse,{Tag,R}}),
	Domain;

parse(Tag={tag,_ThisTag},{set,R=[_Match|Content]})->
	?dbug({parse,{Tag,R}}),
	Content;

parse(Tag=title,{set,R=[_Title|T]})->
	?dbug({parse,{Tag,R}}),
	T;

parse(Tag=meta_keyword,{set,R=[_|T]})->
	?info({parse,{Tag,R}}),
	T;

parse(Tag=meta_description,{set,R=[_|T]})->
	?info({parse,{Tag,R}}),
	T;

parse(Tag=copyright,{set,R=[_A,_B|T]})->
	?info({parse,{Tag,R}}),
	T;

parse(Tag=phone,{set,R=[A|_T]})->
	?info({parse,{Tag,R}}),
	A;

parse(Tag=company_nr,{set,R=[A|_T]})->
	?info({parse,{Tag,R}}),
	A;

parse(Other,R)->
	?warn({unhandledParse,Other,R}),
	R.

start()->
	start([]).

start(RE)->
        gen_server:start_link(  ?SERVERNAME,
                                [{re,RE}],
                                ?START_OPTIONS).

stop(Server)->
        gen_server:call(Server,die).

init([{re,RE}]) when is_list(RE)->
	init([{re,list_to_binary(RE)}]);

init([{re,RE}]) when is_binary(RE)->
        process_flag(trap_exit,true),
	syslogger:set(?MODULE,info),
	posregex:load_driver(),
	{Reply,State}=i_handle_compile(unknown_re,RE,[],#state{}),
        ?info({started,self(),RE,Reply}),
        {ok,State};

init(Other)->
	?warn({badarg,Other}),
	{stop,{badarg,Other}}.

status(Server)->
	gen_server:call(Server,status).

compile(Server,title)->
	compile(Server,title,?re_title);

compile(Server,email)->
	compile(Server,email,?re_email);

compile(Server,domain)->
	compile(Server,domain,?re_domain);

compile(Server,href)->
	compile(Server,href,?re_href);

compile(Server,url)->
	compile(Server,url,?re_url);

compile(Server,meta_keyword)->
	compile(Server,meta_keyword,?re_meta_kw);

compile(Server,meta_description)->
	compile(Server,meta_description,?re_meta_description);

compile(Server,copyright)->
	compile(Server,copyright,?re_copyright);

compile(Server,phone)->
	compile(Server,phone,?re_phone);

compile(Server,htmltag)->
	compile(Server,htmltag,?re_htmltag);

compile(Server,company_nr)->
	compile(Server,company_nr,?re_company_nr);

compile(Server,Other)->
	compile(Server,unknown_re,Other).

compile(Server,Tag,Regex)->
	compile(Server,Tag,Regex,[extended,icase]).

compile(Server,Tag,Regex,Options) when is_list(Regex)->
	compile(Server,Tag,list_to_binary(Regex),Options);

compile(Server,Tag,Regex,Options) when is_binary(Regex)->
	gen_server:call(Server,{compile,Tag,Regex,Options}).

match(Server,S) when is_list(S)->
	match(Server,list_to_binary(S));

match(Server,S) when is_binary(S)->
	gen_server:call(Server,{match,S}).

extractMatches(Server,S,Max) when is_list(S)->
	extractMatches(Server,list_to_binary(S),Max);

extractMatches(Server,B,Max) when is_binary(B)->
	gen_server:call(Server,{extractMatches,B,Max}).

extractMatchPos(Server,S,Max) when is_list(S)->
	gen_server:call(Server,{extractMatchPos,list_to_binary(S),Max});

extractMatchPos(Server,B,Max) when is_binary(B)->
	gen_server:call(Server,{extractMatchPos,B,Max}).
	
exec(Server,S) when is_list(S)->
	exec(Server,list_to_binary(S));

exec(Server,B) when is_binary(B)->
	gen_server:call(Server,{exec,B}).

%% callbacks


handle_call(die,_,State) ->
        {stop,normal,State};

handle_call(status,_,State=#state{port=P}) when is_port(P) ->
        {reply,{ok,erlang:port_info(P),State},State};

handle_call(status,_,State)->
        {reply,{ok,State},State};

handle_call({compile,Tag,RE,_Options},_,State)->
	?info({compile,ignoring_options}),
	handle_compile(Tag,RE,State);

handle_call(M={match,S},_,State=#state{port=Port}) when is_port(Port),is_binary(S)->
	Matches=posregex:match(Port,S,[]),
	?info({M,{reply,Matches}}),
        {reply,Matches,State};

%% we need to parse the entire body passed since only the first match is returned

handle_call({extractMatches,Bin,Max},_,State=#state{port=Port,tag=Tag}) when is_port(Port),is_binary(Bin)->
	Reply=getMatches(Port,Bin,Max),
	PF=State#state.parsefun,
	Reply2=lists:map(fun(Z)->PF(Z) end,Reply),
        {reply,{Tag,Reply2},State};

handle_call({extractMatchPos,Bin,Max},_,State=#state{port=Port,tag=Tag}) when is_port(Port),is_binary(Bin)->
	Reply=getMatchPos(Port,Bin,Max),
        {reply,{Tag,Reply},State};

handle_call(M={exec,S},_,State=#state{port=Port,tag=Tag}) when is_port(Port),is_binary(S)->
	Matches=posregex:exec(Port,S,[]),
	?info({M,{reply,Matches}}),
	Reply=parseMatch(Matches,S),
        {reply,{Tag,Reply},State};

handle_call(Msg,_,State) ->
        ?dbug({unhandledCall,Msg,State}),
        {reply,error,State}.

handle_cast(Msg,State)->
        ?dbug({unhandledCast,Msg,State}),
        {noreply,State}.

handle_info(Msg,State)->
        ?dbug({unhandledInfo,Msg,State}),
        {noreply,State}.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.

terminate(Reason,State=#state{port=P}) when is_port(P)->
	posregex:free(P),
        ?info({stopping,Reason,State}),
        ok;

terminate(Reason,_State)->
        ?info({stopping,Reason}),
        ok.

handle_compile(Tag,RE,State=#state{port=P}) when is_port(P)->
	%% Options must be in the order shown below (bug in posregex???)
	unlink(P),
	port_close(P),
	exit(P,stop),
	{Reply,NewState}=i_handle_compile(Tag,RE,[],State#state{port=undefined}),
	{reply,Reply,NewState};

handle_compile(Tag,RE,State)->
	{Reply,NewState}=i_handle_compile(Tag,RE,[],State),
	{reply,Reply,NewState}.

i_handle_compile(Tag,RE,_Options,State)->
	Options=[icase,extended],
	?info({recompile,RE}),
	case posregex:compile(RE,Options) of
	{ok,P}->
		?dbug({compiled_ok,RE,Options}),
		F=fun(Z)->parse(Tag,Z) end,
		{{ok,Tag},State#state{port=P,tag=Tag,re=RE,parsefun=F}};

	E={error,_Reason}->
		?warn({compile_error,E,RE}),
		{E,State};

	Other->
		?warn({unhandled,Other}),
		{{error,Other},State}
	end.

getMatches(P,Bin,MaxM)->
	getMatches(P,Bin,[],0,MaxM).

getMatches(_P,<<>>,Acc,N,_MaxM)->
	?dbug({exhausted,{result,Acc},{number,N}}),
	Acc;

getMatches(_P,_Bin,Acc,N,MaxM) when N==MaxM->
	?dbug({matchlimit,MaxM}),
	Acc;

getMatches(P,Bin,Acc,N,MaxM)->
	Matches=posregex:exec(P,Bin,[]),
	?dbug({reply,Matches}),
	{Acc0,Remainder,Num}=
	case Matches of
	M={ok,[{Start,End}|_Rest]}->
		PM=parseMatch(M,Bin),
		?dbug({matches,PM}),
		Length=End-Start,
		<<_Unmatched:Start/binary,_Match:Length/binary,Rest/binary>> = Bin,
		{lists:append([Acc,[{set,PM}]]),Rest,N+1};

	NM={error,nomatch}->
		?dbug(NM),
		{Acc,<<>>,N};

	E={error,_Reason}->
		?warn(E),
		{Acc,<<>>,N}
	end,
	getMatches(P,Remainder,Acc0,Num,MaxM).


getMatchPos(P,Bin,MaxM)->
	getMatchPos(P,Bin,[],0,MaxM).

getMatchPos(_P,<<>>,Acc,N,_MaxM)->
	?dbug({exhausted,{result,Acc},{number,N}}),
	Acc;

getMatchPos(_P,_Bin,Acc,N,MaxM) when N==MaxM->
	?warn({matchlimit,MaxM}),
	?dbug({exhausted,{result,Acc}}),
	Acc;

getMatchPos(P,Bin,Acc,N,MaxM)->
	Matches=posregex:exec(P,Bin,[]),
	?dbug({reply,Matches}),
	{Acc0,Remainder,Num}=
	case Matches of
	_M={ok,[Pos={Start,End}|_Rest]}->
		?dbug({pos,Pos}),
		Length=End-Start,
		<<Unmatched:Start/binary,Match:Length/binary,Rest/binary>> = Bin,
		?dbug({{unmatched,Unmatched},{match,Match},{rest,Rest}}),
		{list_to_binary([Acc,<<" ">>,Unmatched]),Rest,N+1};

	NM={error,nomatch}->
		?dbug(NM),
		{Acc,<<>>,N};

	E={error,_Reason}->
		?warn(E),
		{Acc,<<>>,N}
	end,
	getMatchPos(P,Remainder,Acc0,Num,MaxM).

parseMatch(E={error,_Reason},_S)->
	?info(E),
	E;

parseMatch(_R={ok,Matches},B) when is_list(Matches)->
	lists:map(fun(Z)->parseMatch(Z,B) end,Matches);

parseMatch({Start,End},Bin)->
	%%S=binary_to_list(Bin),
	?dbug({match,{start,Start},{last,End},{content,Bin}}),
	Length=End-Start,
        <<_Unmatched:Start/binary,Match:Length/binary,_Rest/binary>> = Bin,
	Match.
