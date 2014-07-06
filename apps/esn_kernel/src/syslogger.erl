-module(syslogger).

-define(TMOUT,30000).
-include("debug.hrl").

%%TBD - add support for override, i.e. syslogger:override(debug)
%% will log ALL debug messages, independent of module
%% need to add override/1

-behaviour(gen_server).

-export([	init/1,
                handle_call/3,
                handle_cast/2,
                handle_info/2,
		code_change/3,
                terminate/2
                ]).

-export([	start_link/0,
		start_link/1,
		status/0,
                stop/0]).

-export([	set/2,
		add/2,
		delete/1,
		getDefaultSyslogLevel/1,
		loadApplicationDefaults/1,
		default/0,
		info/2,
		debug/2,
		warn/2,
		critical/2,
		trace/2,
		list/0,		%% list all Module/Trace Levels
		only/1,		%% log messages only from Module
		getOnly/0,
		setOnly/1,
		override/1,	%% log all messages of given severity
		all/0		%% log all messages
		]).


-define(START_OPTIONS,          []).
-define(SERVERNAME,             ?MODULE).
-define(SERVERACCESS,           {local,?SERVERNAME}).
-define(SERVERCALL,             ?SERVERNAME).

-record(state,{	ttab,
		override=[],
		only=[]}).

code_change(_OldVsn,State,_Extra)->
	{ok,State}.


start_link()->
	start_link([]).

start_link(Arg)->
        gen_server:start_link(  ?SERVERACCESS,
                                ?SERVERNAME,
                                [Arg],
                                ?START_OPTIONS).

stop()->
        gen_server:call(?SERVERCALL,die).

%% External API

only(Module)->
	%% alias
	setOnly(Module).

setOnly(Module)->
	gen_server:call(?SERVERCALL,{setOnly,Module}).

getOnly()->
	gen_server:call(?SERVERCALL,getOnly).

all()->
	gen_server:call(?SERVERCALL,all).

override(clear)->
	gen_server:call(?SERVERCALL,{override,clear});

override(Level) when is_atom(Level)->
	L=level(Level),
	gen_server:call(?SERVERCALL,{override,L}).

default()->
	gen_server:call(?SERVERCALL,default).

level(dbug)->
	?TRACE_DEBUG;

level(debug)->
	?TRACE_DEBUG;

level(info)->
	?TRACE_INFO;

level(warn)->
	?TRACE_WARN;

level(critical)->
	?TRACE_CRITICAL;

level(_Other)->
	level(debug).

list()->
	gen_server:call(?SERVERCALL,list).

loadApplicationDefaults(Application)->
	gen_server:call(?SERVERCALL,{loadApplicationDefaults,Application}).

set(Module,Level) when is_list(Module)->
	set(list_to_atom(Module),Level);

set(Module,Level) when is_list(Level)->
	set(Module,list_to_integer(Level));

set(Module,Level) when is_atom(Level)->
	add(Module,level(Level));

set(Module,Level) when is_integer(Level)->
	add(Module,Level).

add(Module,TraceLevel)->
	gen_server:call(?SERVERCALL,{add,Module,TraceLevel},?TMOUT).

delete(Module)->
	gen_server:call(?SERVERCALL,{delete,Module}).

debug(Module,Message)->
	gen_server:cast(?SERVERCALL,{log,?TRACE_DEBUG,Module,Message}).

trace(Module,Message)->
	gen_server:cast(?SERVERCALL,{log,?TRACE_DEBUG,Module,Message}).

info(Module,Message)->
	gen_server:cast(?SERVERCALL,{log,?TRACE_INFO,Module,Message}).

warn(Module,Message)->
	gen_server:cast(?SERVERCALL,{log,?TRACE_WARN,Module,Message}).

critical(Module,Message)->
	gen_server:cast(?SERVERCALL,{log,?TRACE_CRITICAL,Module,Message}).

init(_Args)->
	process_flag(trap_exit,true),
	Ttab=ets:new(ttab,[set,private]),
	{ok,#state{ttab=Ttab}}.

status()->
	gen_server:call(?SERVERCALL,status).

handle_call(die,_,State) ->
        {stop,normal,State};

handle_call(status,_,State) ->
        {reply,{ok,State},State};

handle_call(default,_,State=#state{ttab=TTab}) ->
	%% iterate through the ets table and reset the levels to the
	%% defaults as returned by getDefaultSyslogLevel/1

	L=ets:tab2list(TTab),
	lists:map(fun(Z)->i_default(Z,TTab) end,L),

        {reply,ok,State};

handle_call({loadApplicationDefaults,Application},_,State=#state{ttab=TTab})->
	%% extract all the modules defined within an application
	Reply=
	case application:get_key(Application,modules) of
	{ok,Modules} when is_list(Modules)->
		lists:map(fun(Z)->setDefaultSyslogLevel(Z,TTab) end, Modules);	
	Other->
		Other
	end,
	{reply,Reply,State};

handle_call({add,Module,TraceLevel},_From,State=#state{ttab=TTab}) ->
	Reply=setLogLevel(Module,TraceLevel,TTab),
        {reply,Reply,State};

handle_call({delete,Module},_From,State=#state{ttab=TTab}) ->
	Reply=ets:delete(TTab,Module),
        {reply,Reply,State};

handle_call(list,_From,State=#state{ttab=TTab}) ->
	Reply=ets:tab2list(TTab),
        {reply,Reply,State};

handle_call({override,clear},_From,State) ->
        {reply,ok,State#state{override=[]}};

handle_call({override,Level},_From,State) ->
        {reply,ok,State#state{override=Level}};

handle_call({setOnly,Module},_From,State) ->
        {reply,ok,State#state{only=Module}};

handle_call(getOnly,_From,State=#state{only=Module}) ->
        {reply,Module,State};

handle_call(all,_From,State) ->
        {reply,ok,State#state{only=[]}};

handle_call(_Msg,_,State) ->
        {reply,ok,State}.

handle_cast({log,Level,Module,Message},State=#state{only=Module})->
	%% we are only interested in messages from module Module
	handle_log(Level,Module,Message,State),
        {noreply,State};

handle_cast({log,Level,Module,Message},State=#state{only=[]})->
	%% all messages are being handled
	handle_log(Level,Module,Message,State),
        {noreply,State};

handle_cast({log,_Level,_Module,_Message},State)->
        {noreply,State};

handle_cast(_Msg,State)->
        {noreply,State}.

handle_info({'EXIT',_Pid,_Reason},State)->
        {noreply,State};

handle_info(_Msg,State)->
        {noreply,State}.

terminate(_Reason,_State) ->
        ok.

i_default({Module,_},TTab)->
	{ok,D}=getDefaultSyslogLevel(Module),
	setLogLevel(Module,D,TTab).
	

%% output function

handle_log(Level,Module,Message,#state{override=OLevel}) when is_integer(OLevel)->
	%% if override is set to a particular level, then all
	%% messages of that level are to be logged
	i_handle_log(Level,[{Module,OLevel}],Module,Message);

handle_log(Level,Module,Message,#state{ttab=TTab})->
	LogLevel=
	case ets:lookup(TTab,Module) of
	[]->
		%% no log level has been set
		{ok,LV}=getDefaultSyslogLevel(Module),
		setLogLevel(Module,LV,TTab),
		LV;
	L->
		L
	end,
	i_handle_log(Level,LogLevel,Module,Message).

i_handle_log(Level,[],Module,Message)->
	%% no module tracel level has been set
	i_trace(Level,Module,Message);

i_handle_log(Level,[{_,LogLevel}],Module,Message) when Level>=LogLevel->
	i_trace(Level,Module,Message);

i_handle_log(_Level,_LogLevel,_Module,_Message)->
	ignore.

i_trace(?TRACE_CRITICAL,Module,Detail)->
        esn_report:critical(Module,Detail);

i_trace(?TRACE_WARN,Module,Detail)->
        esn_report:warn(Module,Detail);

i_trace(?TRACE_INFO,Module,Detail)->
        esn_report:info(Module,Detail);

i_trace(?TRACE_DEBUG,Module,Detail)->
        esn_report:debug(Module,Detail);

i_trace(_,Module,Detail)->
        esn_report:trace(Module,Detail).

getDefaultSyslogLevel(Module)->
	case (catch apply(Module,module_info,[attributes])) of
	KVL when is_list(KVL)->
		case lists:keysearch(sysloglevel,1,KVL) of
                {value,{sysloglevel,[LV|_]}}->
                        {ok,LV};
                false->
                        {ok,?TRACE_DEFAULT}
                end;

	Other->
		{error,Other}
	end.

setDefaultSyslogLevel(Module,TTab)->
	case getDefaultSyslogLevel(Module) of
	{ok,L}->
		setLogLevel(Module,L,TTab);
	_->
		ignored
	end.

setLogLevel(Module,Level,TTab)->
	esn_report:info(Module,{set,Level}),
	ets:insert(TTab,{Module,Level}).
