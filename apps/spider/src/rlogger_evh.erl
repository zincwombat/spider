-module(rlogger_evh).
-behavior(gen_event).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").


-export([start/0,
	 start_link/0,
	 stop/0,
	 init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
	 trace_level/1,
         terminate/2
        ]).

-export([notify/1]).

-record(state, {}).


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

notify(Message)->
	gen_event:notify(?MODULE,Message).

trace_level(Level)->
        gen_event:call(?SERVERNAME,?SERVERNAME,{trace_level,Level}).

init(_)->
	?info({started,self()}),
	{ok,#state{}}.

handle_event(Event,State)->
	?dbug({handleEvent,Event}),
	%% this is where we hook into a formatting module and write the
	%% output in such a format that it can be used to load databases
	%% etc.
	catch rlogger:write(Event),
	{ok,State}.

handle_info(Msg,State)->
	?dbug({unhandledInfo,Msg}),
	{ok,State}.

handle_call(Call,State)->
	?dbug({unhandledCall,Call}),
	{ok,ok,State}.

terminate(_Reason,_State)->
	ok.

