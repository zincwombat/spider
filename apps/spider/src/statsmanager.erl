-module(statsmanager).
-behaviour(gen_server).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").


%%% SASL API
-export([	init/1,
		handle_call/3,
		handle_cast/2,
	 	handle_info/2,
		terminate/2]).


%%% USER API
-export([	start/0,
		stop/0,
		status/0
		]).

-export([
		get/1,		%% get all stats for Module
		get/2,		%% get Module.Key
		reset/1,	%% clear all stats for Module

		set/3,		%% set Module.key=Value
		inc/2,		%% increment Module.key by 1
		inc/3		%% increment Module.key by Value

		

	]).


%%% DEFINITIONS

-define(DEFAULT_TIMEOUT, 	5000).
-define(START_OPTIONS,		[]).

-define(SERVERNAME,		statsmanager).
-define(SERVERCALL,		{local,statsmanager}).


%%% RECORDS

-record(state, {stab}).

%%% INIT FUNCTIONS

start()->
	gen_server:start_link(	?SERVERCALL,
				?SERVERNAME,
				[],
				?START_OPTIONS).

stop()->
	gen_server:call(?SERVERNAME,die).

init(_) ->
	process_flag(trap_exit,true),

	STab=ets:new(stab,[set,private]),

	{ok,#state{stab=STab}}.

status()->
	gen_server:call(?SERVERNAME,status).

get(Module)->
	gen_server:call(?SERVERNAME,{get,Module}).

get(Module,Key)->
	gen_server:call(?SERVERNAME,{get,Module,Key}).

set(Module,Key,Value)->
	gen_server:cast(?SERVERNAME,{set,Module,Key,Value}).

inc(Module,Key)->
	inc(Module,Key,1).

inc(Module,Key,Value)->
	gen_server:cast(?SERVERNAME,{inc,Module,Key,Value}).

reset(Module)->
	gen_server:cast(?SERVERNAME,{reset,Module}).

reset(Module,Key)->
	gen_server:cast(?SERVERNAME,{reset,Module,Key}).


%%% INTERNAL FUNCTIONS

handle_call(die,_,State)->
	{stop,normal,ok,State};

handle_call(status,_,State=#state{})->
	{reply,{ok,State},State};

handle_call(Msg,{_Source,_Tag},State)->
	?warn({unhandled,Msg}),
	{reply,ok,State}.

handle_cast(_Msg,State) ->
	{noreply,State}.

handle_info(_Msg,State) ->
	{noreply,State}.

terminate(_Reason,State=#state{}) ->
	ok.
