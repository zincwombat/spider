-module(configdb_evh).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").
-behavior(gen_event).

%% handled events are of the form:
%%	{Source,{deleted,{Module,Key}}}
%%	{Source,{added,{Module,Key},Value}}
%%	{Source,{changed,{Module,Key},{{old,OldValue},{new,NewValue}}}}

%% where Module is (usually) cconfigdb


-export([start/0,
	 start_link/0,
	 stop/0,
	 init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2
        ]).

-export([subscribe/0,	%% subscribe to all events
	 subscribe/1,	%% subscribe to events associated with a Module
	 subscribe/2,	%% subscribe to a particular event only
	 status/0,	%% show status
	 notify/1]).

-record(state, {stab}).


-define(SERVERNAME,	?MODULE).
-define(SERVERACCESS,	{local,?SERVERNAME}).
-define(ALL,		'_all_').

start_link()->
	start().

start()->
	Reply=gen_event:start_link(?SERVERACCESS),
	gen_event:add_handler(?SERVERNAME,?MODULE,[]),
	Reply.

stop()->
	gen_event:stop(?SERVERNAME).

%% PUBLIC API ------------------------------------------------------------------

subscribe()->
	%% subscribe to all events
	catch gen_event:call(?SERVERNAME,?MODULE,{subscribe,self()}).

subscribe(Module)->
	%% subscribe to all events associated with module Module
	catch gen_event:call(?SERVERNAME,?MODULE,{subscribe,self(),Module}).

subscribe(Module,Key)->
	%% subscribe to all events associated with module Module and
	%% Key
	catch gen_event:call(?SERVERNAME,?MODULE,{subscribe,self(),Module,Key}).

status()->
	catch gen_event:call(?SERVERNAME,?MODULE,status).

notify(Message)->
	catch gen_event:notify(?MODULE,Message).

init(_)->
	?dbug({started,self()}),
	STab=ets:new(stab,[set,private]),
	{ok,#state{stab=STab}}.

sendEvent(STab,Event)->
	Subscribers=getSubscribers(Event,STab),
	i_sendEvent(Subscribers,Event).

i_sendEvent(Subscribers,Event) when is_list(Subscribers)->
	lists:foreach(fun(Z)->i_sendEvent(Z,Event) end,Subscribers);

i_sendEvent(Pid,Event) when is_pid(Pid)->
	Pid ! {event,Event};

i_sendEvent(_,_)->
	ignore.

%% routines to extract the Pids of subscriber processes which have registered
%% interest in the type of event received

moduleKey({cconfigdb,{_Type,{Module,Key},_Detail}})->
	%% this handles add and change events
	{Module,Key};

moduleKey({cconfigdb,{_Type,{Module,Key}}})->
	%% this handles delete events
	{Module,Key};

moduleKey(_)->
	%% this handles all other types of event
	{?ALL,?ALL}.

getSubscribers(Event,STab)->
	{Module,Key}=moduleKey(Event),
	%% get all subscribers who have subscribed to all events
	AllSubs=ets:match(STab,{'$1',?ALL,?ALL}),
	%% get all subscribers who have subscribed to events matching Module
	ModSubs=ets:match(STab,{'$1',Module,?ALL}),
	%% get all subscribers who have subscribed to events matching Module
	%% and Key
	KeySubs=ets:match(STab,{'$1',Module,Key}),
	lists:append([AllSubs,ModSubs,KeySubs]).

handle_event(Event={_Module,_E},State=#state{stab=STab})->
	%% now we need to send this event to all of the
	%% subscribers which have registered for it
	sendEvent(STab,Event),
	{ok,State};

handle_event(Event,State)->
	{ok,State}.

handle_info(Msg={'EXIT',Pid,_Reason},State=#state{stab=STab})->
	ets:delete(STab,Pid),
	{ok,State};

handle_info(Msg,State)->
	{ok,State}.

handle_call({subscribe,Pid},State=#state{stab=STab}) when is_pid(Pid)->
	ets:insert(STab,{Pid,?ALL,?ALL}),
	link(Pid),
	{ok,ok,State};

handle_call({subscribe,Pid,Module},State=#state{stab=STab}) when is_pid(Pid)->
	ets:insert(STab,{Pid,Module,?ALL}),
	link(Pid),
	{ok,ok,State};

handle_call({subscribe,Pid,Module,Key},State=#state{stab=STab}) when is_pid(Pid)->
	ets:insert(STab,{Pid,Module,Key}),
	link(Pid),
	{ok,ok,State};

handle_call(status,State=#state{stab=STab})->
	{ok,ets:tab2list(STab),State};

handle_call(Call,State)->
	{ok,ok,State}.

terminate(_Reason,_State)->
	ok.

