-module(controller_sup).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-behaviour(supervisor).

%% External exports
-export([
		start/0,
		start/2,
		stop/1
	]).

-export([init/1]).


start()->
	start([],[]).

start(_Type,_StartArgs) -> 
	supervisor:start_link({local,controller_sup},controller_sup, []).

stop(_State)->
 	ok.
 
init([]) ->
	SupervisorType=one_for_all,

	MaxRestarts=5,
	MaxRestartInterval=3600,

	SupFlags={SupervisorType,MaxRestarts,MaxRestartInterval},

	Config=	{cconfigdb,
			{cconfigdb,start,[]},
				permanent,20000,worker,[]},

	Controller={controller,
			{controller,start,[]},
				transient,20000,worker,[]},

	ConfigDB_EVH={configdb_evh,
			{configdb_evh,start,[]},
				transient,20000,worker,[]},

	RLogger_EVH={rlogger_evh,
			{rlogger_evh,start,[]},
				transient,20000,worker,[]},

	RLogger={rlogger,
			{rlogger,start,[]},
				transient,20000,worker,[]},

	Datasource_EVH={datasource_evh,
			{datasource_evh,start,[]},
				transient,20000,worker,[]},

	Controller_EVH={controller_evh,
			{controller_evh,start,[]},
				transient,20000,worker,[]},

	Datasource={datasource,
			{datasource,start,[]},
				transient,20000,worker,[]},

	CManager={cmanager,
			{cmanager,start,[]},
				transient,20000,worker,[]},

	Filesource={filesource,
			{filesource,start,[]},
				transient,20000,worker,[]},

	MCache={mcache,
			{mcache,start,[]},
				transient,20000,worker,[]},

	REGEX_Controller={regex_title,
			{regex_controller,start,[]},
				transient,20000,worker,[]},

	%% NOTES on startup sequence:

	Children=
		[
		ConfigDB_EVH,
		Config,
		Controller_EVH,
		Datasource_EVH,
		RLogger_EVH,
		MCache,
		REGEX_Controller,
		CManager,
		Controller,
		Filesource,
		Datasource,
		RLogger],

  	{ok,{SupFlags,Children}}.
