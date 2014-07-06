-module(controller_evh).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").
-behavior(gen_event).

-export([start/0,
	 start_link/0,
	 stop/0,
	 init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2
        ]).

-export([subscribe/0,
	 notify/1]).

-record(state, {stab}).

-include("schema.hrl").
-include("agent.hrl").
-include("monitor.hrl").

-define(SERVERNAME,	?MODULE).
-define(SERVERACCESS,	{local,?SERVERNAME}).

start_link()->
	start().

start()->
	Reply=gen_event:start_link(?SERVERACCESS),
	gen_event:add_handler(?SERVERNAME,?MODULE,[]),
	Reply.

stop()->
	gen_event:stop(?SERVERNAME).

subscribe()->
	gen_event:call(?SERVERNAME,?MODULE,{subscribe,self()}).

notify(Message)->
	gen_event:notify(?MODULE,Message).

init(_)->
	?dbug({started,self()}),
	STab=ets:new(stab,[set,private]),
	{ok,#state{stab=STab}}.

sendEvent(STab,Event)->
        Subscribers=ets:tab2list(STab),
        i_sendEvent(Subscribers,Event).

i_sendEvent(Subscribers,Event) when is_list(Subscribers)->
        lists:foreach(fun(Z)->i_sendEvent(Z,Event) end,Subscribers);

i_sendEvent({Pid},Event) when is_pid(Pid)->
        Pid ! {event,Event}.

handle_event(Event,State=#state{stab=STab})->
	Pids=ets:tab2list(STab),
        sendEvent(STab,Event),
	{ok,State}.

handle_info(Msg={'EXIT',Pid,Reason},State=#state{stab=STab})->
	ets:delete(STab,Pid),
	{ok,State};

handle_info(Msg,State)->
	{ok,State}.

handle_call({subscribe,Pid},State=#state{stab=STab}) when is_pid(Pid)->
	ets:insert(STab,{Pid}),
	link(Pid),
	{ok,ok,State};

handle_call(Call,State)->
	{ok,ok,State}.

terminate(Reason,State)->
	ok.

