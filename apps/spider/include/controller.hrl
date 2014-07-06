-include("xmlinfo.hrl").

-undef(THIS_XMLNS).
-define(THIS_XMLNS,             xmlinfo=?XML(?URN_CC)).

-define(CC_SYNC_TMOUT,                  60000).
-define(CC_STOP_TMOUT,                  60000).
                                                                                
                                                                                
-record(cc_state,{
                agent_tab,      %% ETS table reference {AgentID,Status}
                agent_ptab,     %% ETS table reference {Pid,AgentID}
                aconf,          %% current agent_request data
                cconf,          %% controller configuration
                curagents,      %% number of registered agents
                start_tm,
                stop_tm
                }).

-record(cc_info,{
		?THIS_XMLNS,
		agentConf,	%% agent configuration
		controllerConf,	%% controller configuration
		curAgents,	%% current agents
		start_tm,
		stop_tm}).

-record(cc_conf,{
		?THIS_XMLNS,
		numAgents,	
		maxAgents,
		res_ns,		%% controller configuration
		res_alt_ns	%% current agents
		}).

-record(cc_agentConf,		{?THIS_XMLNS,agentConf}).
-record(cc_controllerConf,	{?THIS_XMLNS,controllerConf}).
-record(cc_curAgents,		{?THIS_XMLNS,curAgents}).
-record(cc_start_tm,		{?THIS_XMLNS,start_tm}).
-record(cc_stop_tm,		{?THIS_XMLNS,stop_tm}).

-record(cc_numAgents,		{?THIS_XMLNS,numAgents}).
-record(cc_maxAgents,		{?THIS_XMLNS,maxAgents}).
-record(cc_res_ns,		{?THIS_XMLNS,res_ns}).
-record(cc_res_alt_ns,		{?THIS_XMLNS,res_alt_ns}).

-undef(THIS_XMLNS).
