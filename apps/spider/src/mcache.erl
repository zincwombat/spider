-module(mcache).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

%% TBD - have a maximum set of nodes and port ranges, don't just add one
%% TBD - monitor for nodeup/nodedown events

-behaviour(gen_server).

-export([	init/1,
                handle_call/3,
                handle_cast/2,
                handle_info/2,
                terminate/2
                ]).

-export([	start/0,
		start_link/0,
                start/1,
		status/0,
                stop/0]).

-export(	[
		get/1,
		set/2
		 ]).

-define(START_OPTIONS,          []).
-define(SERVERNAME,             ?MODULE).
-define(SERVERACCESS,           {local,?SERVERNAME}).
-define(SERVERCALL,             ?SERVERNAME).
-define(CACHE_TMOUT,		1800).	%% 30 mins

-sysloglevel(?TRACE_WARN).

-record(state,{mtab}).

	
status()->
	gen_server:call(?SERVERCALL,status).


start_link()->
	start().

start()->
	start([]).

start(Arg)->
        gen_server:start_link(  ?SERVERACCESS,
                                ?SERVERNAME,
                                [Arg],
                                ?START_OPTIONS).

stop()->
        gen_server:call(?SERVERCALL,die).

get(MailServer)->
	case (catch gen_server:call(?SERVERCALL,{get,MailServer})) of
	{'EXIT',Reason}->
		[];
	Other->
		Other
	end.

set(MailServer,Status)->
	catch gen_server:cast(?SERVERCALL,{set,MailServer,Status}).


init(_)->
	process_flag(trap_exit,true),
	?info({started,self()}),
	MTab=ets:new(mtab,[set,public,named_table]),
	{ok,#state{mtab=MTab}}.	

handle_call({get,MailServer},_,State=#state{mtab=MTab})->

%% format of MailServer is [], [{Prio,MailServer},..]

	%% logic should be:
	%% 1) check if result is in cache
	%% 2) check if timestamp is current
	%% 3) if NOT in cache, perform the request and update the cache
	%% 4) if IN cache, return the cached result

	{_,Now,_}=erlang:now(),

	case ets:lookup(MTab,MailServer) of
	[]->
		?dbug({cacheMiss,MailServer}),
		{reply,[],State};

	[{MailServer,Status,LastUpdate}]->
		Elapsed=Now-LastUpdate,
		if Elapsed < ?CACHE_TMOUT->
			?dbug({cacheHitCurrent,{MailServer,Status}}),
			{reply,Status,State};
		true->
			?dbug({cacheHitExpired,{MailServer,Status}}),
			{reply,[],State}
		end
	end;


handle_call(die,_,State) ->
        {stop,normal,State};

handle_call(status,_,State) ->
        {reply,ets:tab2list(State#state.mtab),State};

handle_call(Msg,_,State) ->
        ?dbug({unhandledCall,Msg,State}),
        {reply,ok,State}.

handle_cast({set,MailServer,Status},State=#state{mtab=MTab})->
	{_,Sec,_}=erlang:now(),
	?dbug({cacheUpdate,{MailServer,Status,Sec}}),
	ets:insert(MTab,{MailServer,Status,Sec}),
	{noreply,State};

handle_cast(Msg,State)->
        ?dbug({unhandledCast,Msg,State}),
        {noreply,State}.

handle_info({'EXIT',Pid,Reason},State)->
        ?dbug({receivedEXIT,{Pid,Reason}}),
        {noreply,State};

handle_info(Msg,State)->
        ?dbug({unhandledInfo,Msg,State}),
        {noreply,State}.

terminate(Reason,State)->
	?info({stopping,Reason}),
	ok.
