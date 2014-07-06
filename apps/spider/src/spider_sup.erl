-module(spider_sup).
-behaviour(supervisor).
%%-include("yaws_config.hrl").
-include("spider_config.hrl").
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SPIDER_SUPERVISOR_NAME}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	CList=[
		?CHILD(configdb_evh,worker),
		?CHILD(cconfigdb,worker),
		?CHILD(controller_evh,worker),
		?CHILD(datasource_evh,worker),
		?CHILD(rlogger_evh,worker),
		?CHILD(mcache,worker),
		?CHILD(regex_controller,worker),
		?CHILD(cmanager,worker),
		?CHILD(geoip,worker),
		?CHILD(controller,worker),
		?CHILD(filesource,worker),
		?CHILD(datasource,worker),
		?CHILD(rlogger,worker)
	],
    	{ok, { {one_for_one, 5, 10}, CList} }.
