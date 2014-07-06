-module(regex_controller).
-behaviour(gen_server).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

%% This module implements a regular expression server

%% CONFIGDB Parameters --------------------------------------------------------


-export([       init/1,
                handle_call/3,
                handle_cast/2,
                handle_info/2,
		code_change/3,
                terminate/2
                ]).

-export([       
		start/0,
		start_link/0,
		op/1,
		compile/0,
		compile/1,
		compile/2,
		compile/3,
		extractMatches/2,
		extractMatches/3,
		extractMatchPos/2,
		extractMatchPos/3,
                start/1,
                status/0,
                stop/0]).

-define(START_OPTIONS,          []).
-define(SERVERNAME,             ?MODULE).
-define(SERVERACCESS,           {local,?SERVERNAME}).
-define(SERVERCALL,             ?SERVERNAME).


-include("regex.hrl").

-sysloglevel(?TRACE_WARN).

-record(state,{retab}).

start_link()->
	start().

start()->
	start([]).

start(Args)->
        gen_server:start_link(  ?SERVERACCESS,
                                ?SERVERNAME,
                                [Args],
                                ?START_OPTIONS).

stop()->
        gen_server:call(?SERVERCALL,die).

init(Args)->
        process_flag(trap_exit,true),
	syslogger:set(?MODULE,dbug),
    	%%DrvDir = filename:join([filename:dirname(code:which(posregex)),"..", "priv"]),
    	DrvDir = code:priv_dir(posregex),
	case erl_ddll:load_driver(DrvDir, 'posregex_drv') of
		ok->
        		?info({started,self(),Args}),
			RETab=ets:new(retab,[set,private]),
        		{ok,#state{retab=RETab}};
		
		Error->
			?error(Error),
			{stop,Error}
	end.

status()->
	gen_server:call(?SERVERCALL,status).

%% callbacks

op(Arg)->
	gen_server:call(?SERVERCALL,{op,Arg}).

compile()->
	compile(?INIT_REGEX_LIST).

compile(Tags=[Tag|_]) when is_atom(Tag)->
	lists:map(fun(Z)->compile(Z) end,Tags); 

compile(Tag)->
	gen_server:call(?SERVERCALL,{op,{compile,[Tag]}}).

compile(Tag,RE)->
	gen_server:call(?SERVERCALL,{op,{compile,[Tag,RE]}}).

compile(Tag,RE,Options)->
	gen_server:call(?SERVERCALL,{op,{compile,[Tag,RE,Options]}}).

extractMatches(Tag,Bin)->
	gen_server:call(?SERVERCALL,{op,{extractMatches,Tag,Bin,?DEFAULT_MAX_MATCHES}}).

extractMatches(Tag,Bin,Max)->
	gen_server:call(?SERVERCALL,{op,{extractMatches,Tag,Bin,Max}}).

extractMatchPos(Tag,Bin)->
	gen_server:call(?SERVERCALL,{op,{extractMatchPos,Tag,Bin,?DEFAULT_MAX_MATCHES}}).

extractMatchPos(Tag,Bin,Max)->
	gen_server:call(?SERVERCALL,{op,{extractMatchPos,Tag,Bin,Max}}).

handle_call({op,_Op={compile,CArgs=[Tag|_Rest]}},_From,State=#state{retab=RETab})->
	Reply=
	case ets:lookup(RETab,Tag) of
	[]->
		?info({no_regex,Tag}),
		handle_compile(CArgs,RETab);

	[{Tag,Pid}]->
		?info({recompiling,Tag,Pid}),
		i_handle_compile(Pid,CArgs,RETab);

	Other->
		?warn({ets_other,Other}),
		Other
	end,
	{reply,Reply,State};

handle_call({op,_Op={extractMatches,Tag,Bin,Max}},_From,State=#state{retab=RETab})->
	case ets:lookup(RETab,Tag) of
	[]->
		%% regex has not been compiled
		?info({no_regex,Tag}),
		{reply,{error,{not_compiled,Tag}},State};

	[{Tag,Pid}]->
		?info({extracting,Tag,{handler,Pid}}),
		Reply=i_handle_extract(Pid,Tag,Bin,Max),
		{reply,Reply,State};

	Other->
		?warn({ets_other,Other}),
		{reply,Other,State}
	end;

handle_call({op,_Op={extractMatchPos,Tag,Bin,Max}},_From,State=#state{retab=RETab})->
	case ets:lookup(RETab,Tag) of
	[]->
		%% regex has not been compiled
		?info({no_regex,Tag}),
		{reply,{error,{not_compiled,Tag}},State};

	[{Tag,Pid}]->
		?info({extracting,Tag,{handler,Pid}}),
		Reply=i_handle_extractPos(Pid,Tag,Bin,Max),
		{reply,Reply,State};

	Other->
		?warn({ets_other,Other}),
		{reply,Other,State}
	end;
	

handle_call(die,_,State) ->
        {stop,normal,State};

handle_call(status,_,State=#state{retab=RETab}) ->
	REInfo=ets:tab2list(RETab),
        {reply,{ok,State,{retab,REInfo}},State};

handle_call(Msg,_,State) ->
        ?dbug({unhandledCall,Msg,State}),
        {reply,error,State}.

handle_cast(Msg,State)->
        ?dbug({unhandledCast,Msg,State}),
        {noreply,State}.

handle_info(Msg={'EXIT',Pid,_Reason},State=#state{retab=RETab})->
	%% need to scan REtab for tuple {_Key,Pid}
        ?dbug({linkedExit,Msg,State}),
	case ets:match(RETab,{'$1',Pid}) of 
	[[Key]]->
		?warn({regex_server_exit,{pid,Pid},{tag,Key}}),
		ets:delete(RETab,Key);
	Other->
		?warn({unknown_exit,Other})
	end,
        {noreply,State};

handle_info(Msg,State)->
        ?dbug({unhandledInfo,Msg,State}),
        {noreply,State}.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.

terminate(Reason,State=#state{retab=RETab})->
        ?info({stopping,Reason,State}),
	ets:delete(RETab),
        ok.


%%% compiler interface

handle_compile(CArgs,RETab)->
	case regex_server:start() of
	{ok,Pid}->
		?info({regex_server_started,Pid}),
		i_handle_compile(Pid,CArgs,RETab);
	Other->
		?warn({compile_error,Other}),
		Other
	end.

i_handle_compile(Pid,[Tag,RE,Options],RETab)->
	C=(catch regex_server:compile(Pid,Tag,RE,Options)),
	i_handle_compile2(Pid,C,RETab);

i_handle_compile(Pid,[Tag,RE],RETab)->
	C=(catch regex_server:compile(Pid,Tag,RE)),
	i_handle_compile2(Pid,C,RETab);

i_handle_compile(Pid,[Tag],RETab)->
	C=(catch regex_server:compile(Pid,Tag)),
	i_handle_compile2(Pid,C,RETab).

i_handle_compile2(Pid,{ok,Tag},RETab)->
	?info({compiled,Tag}),
	link(Pid),
	?info({linked,Pid}),
	ets:insert(RETab,{Tag,Pid}),
	{ok,{compiled,Tag}};

i_handle_compile2(_Pid,Other,_RETab)->
	{error,Other}.

%%% extractMatches interface

i_handle_extract(Pid,_Tag,Bin,Max) when is_binary(Bin)->
	_M=(catch regex_server:extractMatches(Pid,Bin,Max));

i_handle_extract(_,_,NotBin,_)->
	{error,{badarg,{type,NotBin}}}.

i_handle_extractPos(Pid,_Tag,Bin,Max) when is_binary(Bin)->
	_M=(catch regex_server:extractMatchPos(Pid,Bin,Max));

i_handle_extractPos(_,_,NotBin,_)->
	{error,{badarg,{type,NotBin}}}.
