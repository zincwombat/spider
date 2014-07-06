-module(controller).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").
-behaviour(gen_fsm).


-export([       init/1,
                handle_event/3,
                handle_sync_event/4,
                handle_info/3,
                terminate/3
]).


-export([
        start/0,
	start_link/0,
        stop/0
]).

-export([	%% FSM STATES
		'IDLE'/2,
		'INIT'/2,
		'READY'/2,
		'RUNNING'/2,
		'WAIT_STOP'/2,
		'SUSPENDING'/2,
		'SUSPENDED'/2,
		'COMPLETED'/2
	]).

-export([	run/0,
		notify_listeners/1,
		notify/1,


		stopAgents/0,		%% stop all registered agents (normal)
		stopAgents/1,		%% stop all registered agents with 
					%% reason
		status/0,
		nstatus/0,
		agentStatus/0,

		agentBroadcast/1,
		agentBroadcast/2,
		initAgents/2,

		setAgentConf/1,		%% set the agent configuration
		getAgentConf/0,		%% get the agent configuration
		setControllerConf/1,	%% set the controller configuration
		getControllerConf/0,	%% get the controller configuration

		addAgents/1,		%% add agents

		suspend/1,		%% suspend nominated agent
		suspend/0,		%% suspend all Agents
		resume/0,		%% resume all suspended Agents
		reset/0,		%% reset all agents
		jobstats/0
		]).


-define(START_OPTIONS,  []).
-define(SERVERNAME,     ?MODULE).
-define(INITTRACELEVEL,	0).

-include("agent.hrl").
-include("urn_agent.hrl").
-include("agent_test.hrl").
-include("controller.hrl").

-sysloglevel(?TRACE_INFO).

initClookup(Key)->
        case cconfigdb:lookup(Key) of
        undefined->
                Reason={configMissing,Key},
                throw({stop,Reason});
        {ok,Val}->
                Val
        end.

sync(Message)->
	sync(Message,?CC_SYNC_TMOUT).

sync(Message,Timeout)->
	gen_fsm:sync_send_all_state_event(?SERVERNAME,Message,Timeout).

broadcast(Message)->
	gen_fsm:send_all_state_event(?SERVERNAME,Message).


reset()->
	sync(reset).

jobstats()->
	sync(jobstats).

notify_listeners(Message)->
	controller_evh:notify({?MODULE,Message}).

notify(Message)->
	gen_fsm:send_event(?SERVERNAME,Message).

%% suspend and resume functions .... only called by cmanager

suspend()->
	gen_fsm:send_event(?SERVERNAME,suspendAllAgents).

suspend(AgentID)->
	gen_fsm:send_event(?SERVERNAME,{suspendAgent,AgentID}).

resume()->
	gen_fsm:send_event(?SERVERNAME,resume).

setAgentConf(AgentConf)->
	sync({setAgentConf,AgentConf}).	

getAgentConf()->
	sync(getAgentConf).	

getControllerConf()->
	sync(getControllerConf).	

setControllerConf(ControllerConf)->
	sync({setControllerConf,ControllerConf}).	

run()->
	gen_fsm:send_event(?MODULE,run).

initAgents(AgentConf,ControllerConf)->
	sync({initAgents,AgentConf,ControllerConf}).

stopAgents()->
	stopAgents(normal).

stopAgents(Reason)->
	gen_fsm:send_all_state_event(?MODULE,{stopAgents,Reason}).

stop(Reason)->
	%% we need to define the type of stop requested
	%% e.g. stop(normal)->stops agents gracefully
	%% stop(now)->exits immediately
	
        sync({stop,Reason}).

status()->
	sync(status).

nstatus()->
	sync(nstatus).

addAgents(Num)->
	sync({addAgents,Num}).

agentStatus()->
	%% return the status of all the agents
	sync(agentStatus).

%% START/INIT =================================================================

start_link()->
	start().

start()->
        gen_fsm:start_link({local,?MODULE},?MODULE,[],[]).

stop()->
        stop(normal).


init(_Args)->
        process_flag(trap_exit,true),

	%% create the ets lookup tables

	ATab=ets:new(atab,[set,private]),
	APTab=ets:new(aptab,[set,private]),

	%% when the process starts, it initialises into the 'IDLE'
	%% state.

	%% before any agents can be started, the #agent_request{} 
	%% record needs to be defined, this is stored in the #cc_state{}
	%% variable.

	?info({started,self()}),

	notify_listeners({state,'IDLE'}),

	{ok,'IDLE',#cc_state{
			agent_tab=ATab,
			agent_ptab=APTab,
			aconf=#agentConf{},
			cconf=#controllerConf{},
			curagents=0}}.

%% UTILITY FUNCTIONS ==========================================================


%% start N agents with agentIDs N,N-1,N-2 .... 1 etc

i_startAgents(0,_AC)->
	ok;
	
i_startAgents(N,AC)->
	%% create the name by which the agent can be addressed

	AgentID=list_to_atom("agent_"++integer_to_list(N)),

	%% set the callback function for the agent to use to
	%% communicate with the dispatcher. This communication
	%% is asynchronous

	i_startAgent(AgentID,AC),
	i_startAgents(N-1,AC).

%% start N agents with IDs J,J+1,J+N-1 ...

i_addAgents(StartID,NumAgents,AC)->
	i_addAgents(StartID,StartID+NumAgents,NumAgents,AC).

i_addAgents(EndID,EndID,_NumAgents,_AC)->
	ok;
	
i_addAgents(ID,EndID,NumAgents,AC)->
	%% create the name by which the agent can be addressed

	AgentID=list_to_atom("agent_"++integer_to_list(ID)),

	%% set the callback function for the agent to use to
	%% communicate with the dispatcher. This communication
	%% is asynchronous

	i_startAgent(AgentID,AC),
	i_addAgents(ID+1,EndID,NumAgents,AC).

%% remove NumAgents starting at StartID and working down ....

i_removeAgents(StartID,NumAgents)->
	i_removeAgents(StartID,NumAgents,0).

i_removeAgents(ThisID,NumAgents,NumAgents)->
	?dbug(allAgentsRemoved),
	ok;
	
i_removeAgents(ThisID,NumAgents,AgentsRemoved)->
	%% create the name by which the agent can be addressed

	AgentID=list_to_atom("agent_"++integer_to_list(ThisID)),
	?dbug({removingAgent,AgentID}),
	agent:stop(AgentID),

	i_removeAgents(ThisID-1,NumAgents,AgentsRemoved+1).


%% start the individual agent

i_startAgent(AgentID,AC)->
	?info({startingAgent,AgentID}),
	CallBack=fun(Z)->gen_fsm:send_event(?SERVERNAME,{AgentID,Z}) end,
	agent:start(AgentID,CallBack,AC).


i_stopAgents(AgentPTab,Type)->
	
	%% convert the AgentTab entries into a list and then for each
	%% element in the list, send a stop command

	Agents=ets:tab2list(AgentPTab),
	lists:foreach(fun(Z)->i_stopAgent(Z,Type) end,Agents).

i_stopAgent({Pid,Agent},normal) when is_pid(Pid)->
	%% gentle stop
	agent:stop(Agent);

i_stopAgent({Pid,_Agent},immediate) when is_pid(Pid)->
	%% brutal stop
	exit(Pid,die);

i_stopAgent({_,_Agent},_Type)->
	%% not active, so ignore
	ignored.

i_resumeAgents(AgentTab,AgentConf)->
	AgentList=ets:tab2list(AgentTab),
	lists:foreach(fun(Z)->i_resumeAgent(Z,AgentConf) end,AgentList).

i_resumeAgent({AgentID,'SUSPENDED'},_AgentConf)->
	?info({resumingAgent,AgentID}),
	agent:resume(AgentID);

i_resumeAgent({AgentID,State},_)->
	?warn({resumeIgnored,AgentID,State}),
	ignore.

i_resetAgents(AgentTab)->
	AgentList=ets:tab2list(AgentTab),
	lists:foreach(fun(Z)->i_resetAgent(Z) end,AgentList).

i_resetAgent({AgentID,S={exit,_}})->
	?warn({agentResetWrongState,AgentID,S}),
	ignore;

i_resetAgent({AgentID,_State})->
	?info({resetAgent,AgentID}),
	agent:reset(AgentID).

i_runAgents(AgentTab)->
	%% send start command to all agents
	AgentList=ets:tab2list(AgentTab),
	lists:foreach(fun(Z)->i_runAgent(Z) end,AgentList).

i_runAgent({AgentID,'IDLE'})->
	?dbug({agentRun,AgentID}),
	i_runAgent(AgentID);

i_runAgent({AgentID,AgentState})->
	%% can only run agents which are in the 'IDLE' state
	?warn({agentRunWrongState,AgentID,AgentState}),
	ignore;

i_runAgent(AgentID)->
	agent:run(AgentID).

i_countAgentState(State,AgentTab)->
	%% count the number of agents in state State
	AgentList=ets:tab2list(AgentTab),
	lists:foldl(fun(Z,Acc)->i_countAgentState(State,Z,Acc) end,0,AgentList).

i_countAgentState(State,{_AgentID,State},Count)->
	Count+1;

i_countAgentState(_State,_,Count)->
	Count.


%% broadcast a message to all agents 

agentBroadcast(Message)->
	broadcast({agentBroadcast,cur_state,Message}).

agentBroadcast(all_state,Message)->
	broadcast({agentBroadcast,all_state,Message}).

%% internal API

i_agentBroadcast(AgentPTab,Type,Message)->

	%% note that we use the AgentPidTable which contains records of
	%% the form {AgentPID,AgentID}

	Agents=ets:tab2list(AgentPTab),
	lists:foreach(fun(Z)->i_agentMessage(Z,Type,Message) end,Agents).

i_agentMessage({_AgentPID,AgentID},cur_state,Message)->
	agent:message(AgentID,Message);

i_agentMessage({_AgentPID,AgentID},all_state,Message)->
	agent:a_message(AgentID,Message).


%% STATE CALLBACKS ============================================================

%%=============================================================================
%% IDLE STATE
%%=============================================================================


'IDLE'({AgentID,#agentStatus{payload={state,{'IDLE',{init,Pid}}}}},SData)->

	%% agent cannot start in this state

	?warn({unexpectedAgentStart,AgentID,{state,'IDLE'}}),
	?warn({killingAgent,AgentID}),
	i_stopAgent({Pid,AgentID},immediate),

	{next_state,'IDLE',SData};

'IDLE'(Msg,SData)->
	?info({unhandledEvent,Msg,{state,'IDLE'}}),
	{next_state,'IDLE',SData}.

%%=============================================================================
%% INIT STATE
%%=============================================================================

'INIT'(Msg={AgentID,#agentStatus{payload={state,{'IDLE',{init,Pid}}}}},
	   SData=#cc_state{agent_ptab=AgentPTab,
			agent_tab=AgentTab,
			cconf=CC=#controllerConf{numagents=NumAgents},
			curagents=CurAgents})->

	NewCurAgents=CurAgents+1,
	?dbug({{state,'INIT'},{msg,Msg}}),

	ets:insert(AgentPTab,{Pid,AgentID}),
	ets:insert(AgentTab,{AgentID,'IDLE'}),

	NextState=
	if NewCurAgents==NumAgents->
		?info({nextstate,'READY'}),
		notify_listeners({state,'READY'}),
		'READY';
	true->
		'INIT'
	end,

	{next_state,
	 NextState,
	 SData#cc_state{curagents=NewCurAgents}};

'INIT'(Msg,SData)->
	?info({unhandledEvent,Msg,{state,'INIT'}}),
	{next_state,'INIT',SData}.

%%=============================================================================
%% READY STATE
%%=============================================================================

'READY'(Msg=run,SData=#cc_state{agent_tab=AgentTab})->

	?dbug({{state,'READY'},{msg,Msg}}),

	%% we need to broadcast the run message to all of the
	%% agents

	i_runAgents(AgentTab),
	notify_listeners({state,'RUNNING'}),
	?info({nextstate,'RUNNING'}),

	{next_state,'RUNNING',SData#cc_state{start_tm=erlang:now()}};

'READY'(Msg={agent_exit,_AgentID,_PrevState,suspended},
        SData=#cc_state{agent_ptab=AgentPTab,curagents=CurAgents})->

	%% if the agent has been suspended, we shouldn't restart it (TBD)

	?dbug({{state,'READY'},{msg,Msg}}),

	if CurAgents==0->
		?info(noAgentsRunning),
		?info({nextstate,'SUSPENDED'}),
		notify_listeners({state,'SUSPENDED'}),
		{next_state,'SUSPENDED',SData};
	true->
		{next_state,'READY',SData}
	end;

'READY'(Msg={suspendAgent,AgentID},SData)->

	?dbug({{state,'READY'},{msg,Msg}}),
	
	agent:suspend(AgentID),
	{next_state,'READY',SData};


'READY'(Msg={agent_exit,AgentID,_PrevState,Reason},
	SData=#cc_state{aconf=AC,
		     cconf=CC=#controllerConf{numagents=NumAgents},
		     curagents=CurAgents})->

	?dbug({{state,'READY'},{msg,Msg}}),

	?warn({unexpectedAgentExit,AgentID,Reason}),

	%% if an agent exits in this state, 
	%% it must be an unexpected exit so we should restart it unless
	%% we already have the maximum number of agents running

	if CurAgents < NumAgents->
		?info({agentStart,AgentID}),
		i_startAgent(AgentID,AC),	
		notify_listeners({state,'INIT'}),
		?info({nextstate,'INIT'}),
		{next_state,'INIT',SData};
	true->
		{next_state,'READY',SData}
	end;


'READY'(Msg={AgentID,#agentStatus{payload={state,{'IDLE',{init,Pid}}}}},
	SData=#cc_state{agent_ptab=AgentPTab,
	             agent_tab=AgentTab,
		     cconf=CC=#controllerConf{numagents=NumAgents},
		     curagents=CurAgents})->

	NewCurAgents=CurAgents+1,

	?warn({{state,'READY'},{msg,Msg}}),

	ets:insert(AgentPTab,{Pid,AgentID}),
	ets:insert(AgentTab,{AgentID,'IDLE'}),

	{next_state,'READY',SData#cc_state{curagents=NewCurAgents}};

'READY'(Msg={AgentID,#agentStatus{payload={state,State}}},
	   SData=#cc_state{agent_tab=AgentTab})->

	?dbug({{state,'READY'},{msg,Msg}}),

	ets:insert(AgentTab,{AgentID,State}),

	{next_state,'READY',SData};

'READY'(Msg,SData)->
	?info({unhandledEvent,Msg,{state,'READY'}}),
	{next_state,'READY',SData}.

%%=============================================================================
%% RUNNING STATE
%%=============================================================================


'RUNNING'(Msg={AgentID,#agentStatus{payload={state,{'IDLE',{init,Pid}}}}},
	  SData=#cc_state{agent_ptab=AgentPTab,
		       agent_tab=AgentTab,
		       curagents=CurAgents})->

	%% an agent has started in state 'RUNNING', so we must 
	%% command the agent to run.

	?dbug({{state,'RUNNING'},{msg,Msg}}),
	NewCurAgents=CurAgents+1,
	i_runAgent(AgentID),

	ets:insert(AgentPTab,{Pid,AgentID}),
	ets:insert(AgentTab,{AgentID,'IDLE'}),

	{next_state,'RUNNING',SData#cc_state{curagents=NewCurAgents}};

'RUNNING'(Msg={AgentID,#agentStatus{payload={state,'FINISHED'}}},
	   SData=#cc_state{agent_tab=AgentTab})->

	%% note: the agent is finished! if all agents which are not
	%% in state 'SUSPENDED' are in state 'FINISHED', then the 
	%% entire job is finished. We can support this by assuming
	%% that an agent in either state WAIT_DATA or WAIT_EXEC is
	%% still running. We can call this metastate 'RUNNING' and 

	?dbug({{state,'RUNNING'},{msg,Msg}}),

	ets:insert(AgentTab,{AgentID,'FINISHED'}),

	case i_countAgentState('RUNNING',AgentTab) of
	0->
		?info(jobComplete),

		%% TBD: should also check the datasource to see if it
		%% has been exhausted
		notify_listeners({state,'COMPLETED'}),
		?dbug({nextstate,'COMPLETED'}),
		{next_state,'COMPLETED',SData#cc_state{stop_tm=erlang:now()}};
	N->
		?dbug({agentFinished,AgentID,{remaining,N}}),
		{next_state,'RUNNING',SData}
	end;

'RUNNING'(Msg={AgentID,#agentStatus{payload={state,State}}},
	  SData=#cc_state{agent_ptab=AgentPTab,
	               agent_tab=AgentTab,
		       curagents=CurAgents})->

	%% an agent has started in state 'RUNNING', so we must 
	%% command the agent to run.

	?dbug({{state,'RUNNING'},{msg,Msg}}),

	ets:insert(AgentTab,{AgentID,State}),

	{next_state,'RUNNING',SData};


'RUNNING'(Msg={suspendAgent,AgentID},SData)->

	?dbug({{state,'RUNNING'},{msg,Msg}}),
	agent:message(AgentID,{control,suspend}),
	{next_state,'RUNNING',SData};

'RUNNING'(Msg=suspendAllAgents,SData=#cc_state{agent_ptab=AgentPTab})->

	?dbug({{state,'RUNNING'},{msg,Msg}}),
	i_agentBroadcast(AgentPTab,cur_state,{control,suspend}),
	notify_listeners({state,'SUSPENDING'}),
	{next_state,'SUSPENDING',SData};

'RUNNING'(Msg={agent_exit,A,_,_Reason},SData=#cc_state{curagents=0})->

	?dbug({{state,'RUNNING'},{msg,Msg}}),
	?warn(noAgentsLeft),
	notify_listeners({state,'IDLE'}),
	{next_state,'IDLE',SData};

'RUNNING'(Msg,SData)->
	?info({unhandledEvent,Msg,{state,'RUNNING'}}),
	{next_state,'RUNNING',SData}.

%%=============================================================================
%% SUSPENDING STATE
%%=============================================================================

'SUSPENDING'(Msg={AgentID,#agentStatus{payload={state,State}}},
	     SData=#cc_state{agent_tab=AgentTab,
			  curagents=CurAgents})->

	?dbug({{state,'SUSPENDING'},{msg,Msg}}),

	%% if all agents are suspended, go to the 'SUSPENDED' state
	%% and notify cmanager

	ets:insert(AgentTab,{AgentID,State}),

	case i_countAgentState('SUSPENDED',AgentTab) of
	CurAgents->
		?info(allAgentsSuspended),
		notify_listeners({state,'SUSPENDED'}),
		{next_state,'SUSPENDED',SData};
	N->
		?dbug({agentsSuspended,{N,CurAgents}}),
		{next_state,'SUSPENDING',SData}
	end;

'SUSPENDING'(Msg,SData)->
        ?info({unhandledEvent,Msg,{state,'SUSPENDING'}}),
        {next_state,'SUSPENDING',SData}.

%%=============================================================================
%% SUSPENDED STATE
%%=============================================================================

'SUSPENDED'(Msg=resume,SData=#cc_state{agent_tab=AgentTab,
				    aconf=AC})->

	?dbug({{state,'SUSPENDED'},{msg,Msg}}),
	i_resumeAgents(AgentTab,AC),
	{next_state,'RUNNING',SData};

'SUSPENDED'(Msg,SData)->
	?info({unhandledEvent,Msg,{state,'SUSPENDED'}}),
	{next_state,'SUSPENDED',SData}.

%%=============================================================================
%% WAIT_STOP STATE
%%	WAIT_STOP::stopped->IDLE
%%=============================================================================

'WAIT_STOP'(Msg={agent_exit,_Agent,_PrevState,_Reason},
	SData=#cc_state{curagents=0})->

	%% no agents left, transit to 'IDLE' state

	?dbug({{state,'WAIT_STOP'},{msg,Msg}}),
	?info(allAgentsExited),
	notify_listeners({state,'IDLE'}),
	?info({nextstate,'IDLE'}),
	{next_state,'IDLE',SData#cc_state{stop_tm=erlang:now()}};

'WAIT_STOP'(Msg={timeout,waitstop},SData=#cc_state{curagents=0})->

	?dbug({{state,'WAIT_STOP'},{msg,Msg}}),
	notify_listeners({state,'IDLE'}),
	?info({nextstate,'IDLE'}),

	{next_state,'IDLE',SData};

'WAIT_STOP'(Msg={timeout,waitstop},SData=#cc_state{agent_ptab=AgentPTab})->

	%% waited too long for agents to stop,
	%% send a signal to kill here ...

	?dbug({{state,'WAIT_STOP'},{msg,Msg}}),
	?warn(killingAgents),

	i_stopAgents(AgentPTab,immediate),

	{next_state,'WAIT_STOP',SData};

'WAIT_STOP'(Msg,SData)->
	?info({unhandledEvent,Msg,{state,'WAIT_STOP'}}),
	{next_state,'WAIT_STOP',SData}.

%%=============================================================================
%% COMPLETED STATE
%%=============================================================================

'COMPLETED'(Msg={AgentID,#agentStatus{payload={state,'IDLE'}}},
	   SData=#cc_state{agent_tab=AgentTab,curagents=CurAgents})->

	?dbug({{state,'COMPLETED'},{msg,Msg}}),

	ets:insert(AgentTab,{AgentID,'IDLE'}),

	case i_countAgentState('IDLE',AgentTab) of
	CurAgents->
		?info(allAgentsIdle),
		notify_listeners({state,'READY'}),
		?info({nextstate,'READY'}),
		{next_state,'READY',SData};
	N->
		?dbug({agentsIdle,{N,CurAgents}}),
		{next_state,'COMPLETED',SData}
	end;

'COMPLETED'(Msg={AgentID,#agentStatus{payload={state,State}}},
	   SData=#cc_state{agent_tab=AgentTab})->

	?dbug({{state,'COMPLETED'},{msg,Msg}}),

	ets:insert(AgentTab,{AgentID,State}),

	{next_state,'COMPLETED',SData};

'COMPLETED'(Msg,SData)->
	?info({unhandledEvent,Msg,{state,'COMPLETED'}}),
	{next_state,'COMPLETED',SData}.

%% initialise the agents and start the job, must be in state 'IDLE'

handle_sync_event(Msg={initAgents, AConf=#agentConf{},
			 	   CConf=#controllerConf{numagents=N}},
		  _From,'IDLE',
		  SData=#cc_state{agent_tab=AgentTab})->

	%% current state is 'IDLE' so there are no agents started
	%% therefore we need to start N agents

	?info({handleSyncEvent,Msg,{state,'IDLE'}}),
	ets:delete_all_objects(AgentTab),
	i_startAgents(N,AConf),
	notify_listeners({state,'INIT'}),
       	{reply,ok,'INIT',SData#cc_state{aconf=AConf,
				     cconf=CConf,
				     curagents=0}};


handle_sync_event(Msg={setAgentConf,AgentConf},_From,State='IDLE',
		  SData=#cc_state{aconf=AgentConf})->


	?info({handleSyncEvent,Msg,{state,State}}),
	?info({agentConfig,no_change}),
        {reply,ok,State,SData#cc_state{aconf=AgentConf}};


handle_sync_event(Msg={setAgentConf,AgentConf},_From,State='IDLE',
		  SData=#cc_state{aconf=OldAgentConf})->

	?info({handleSyncEvent,Msg,{state,State}}),
	?warn({oldAgentConfig,OldAgentConf,newAgentConf,AgentConf}),
	handle_agent_config_change(AgentConf,State,SData);

handle_sync_event(Msg=getAgentConf,_From,State,
		  SData=#cc_state{aconf=AgentConf})->

	?info({handleSyncEvent,Msg,{state,State}}),
        {reply,{ok,AgentConf},State,SData};

handle_sync_event(Msg={setControllerConf,CConf},_From,State,
		  SData=#cc_state{cconf=OldCConf})->

	%% TBD should this be allowed in 'IDLE' state only ???

	?info({handleSyncEvent,Msg,{state,State}}),
	?warn({oldControllerConfig,OldCConf,newControllerConf,CConf}),
	handle_controller_config_change(CConf,State,SData);

handle_sync_event(Msg=getControllerConf,_From,State,
		  SData=#cc_state{cconf=ControllerConf})->

	?info({handleSyncEvent,Msg,{state,State}}),
        {reply,{ok,ControllerConf},State,SData};


handle_sync_event(Msg=status,_From,State,SData=#cc_state{ agent_tab=AgentTab,
						     agent_ptab=AgentPTab})->
	?dbug({handleSyncEvent,Msg,{state,State}}),
        {reply,{SData,{agents_status,ets:tab2list(AgentTab)},
		      {agent_pids,ets:tab2list(AgentPTab)}},State,SData};

handle_sync_event(Msg=nstatus,_From,State,SData=#cc_state{ agent_tab=AgentTab,
						     agent_ptab=AgentPTab})->
	?info({handleSyncEvent,Msg,{state,State}}),

	%% need to add the current STATE

        %%{reply,objectMapper:map(SData),State,SData};
        {reply,SData,State,SData};

handle_sync_event(Msg=agentStatus,_From,State,
		  SData=#cc_state{agent_tab=AgentTab,
			       agent_ptab=AgentPTab})->

	?info({handleSyncEvent,Msg,{state,State}}),
        {reply,{ets:tab2list(AgentTab),
		ets:tab2list(AgentPTab)},State,SData};

handle_sync_event(Msg={addAgents,AddAgents},_From,State,
	SData=#cc_state{aconf=AgentConf,
		     cconf=CC=#controllerConf{numagents=NumAgents},
		     curagents=CurAgents})->

	%% TBD! which states is this valid for???

	%% we need to
	%% 1) increase the MaxAgents
	%% 2) invoke the additional number of agents
	%% request to add a number of agents 

	?info({handleSyncEvent,Msg,{state,State}}),
	?info({addAgents,AddAgents}),

	i_addAgents(NumAgents+1,AddAgents,AgentConf),

        {reply,ok,State,
         SData#cc_state{cconf=CC=#controllerConf{numagents=NumAgents+AddAgents}}};

handle_sync_event(Msg=reset,_From,State,
		  SData=#cc_state{agent_tab=AgentTab})->

	?info({handleSyncEvent,Msg,{state,State}}),
	i_resetAgents(AgentTab),
        {reply,ok,State,SData};

handle_sync_event(Msg=jobstats,_From,State='COMPLETED',
	SData=#cc_state{start_tm={SMS,SS,SUS},
		     stop_tm={EMS,ES,EUS},
		     cconf=CC=#controllerConf{numagents=NumAgents}})->

	?info({handleSyncEvent,Msg,{state,State}}),

	S=(SMS*1000000)+SS+(SUS/1000000),
	F=(EMS*1000000)+ES+(EUS/1000000),
	
        {reply,{jobstats,{elapsed,F-S},
			{agents,NumAgents}},'COMPLETED',SData};

handle_sync_event(Msg,_From,State,SData)->
	?info({unhandledSyncEvent,Msg,{state,State},{sdata,SData}}),
        {reply,ignored,State,SData}.

handle_event(Msg={agentBroadcast,Type,Message},State,SData=#cc_state{agent_ptab=AgentPTab})->
	?info({handleEvent,Msg,{state,State}}),
	i_agentBroadcast(AgentPTab,Type,Message),
        {next_state,State,SData};

handle_event(Msg={Agent,#agentStatus{payload={status,finished}}},State,SData)->
	?info({handleEvent,Msg,{state,State}}),
	%% we can optionally stop the agent here ..
	agent:message(Agent,{control,stop}),
        {next_state,State,SData};

handle_event(Msg={Agent,#agentStatus{payload={state,AgentState}}},State,
	     SData=#cc_state{agent_tab=AgentTab})->

	%% we have received a state update from an Agent, so we must
	%% update the ets table agent_tab {agentID,agentState} accordingly

	?info({handleEvent,Msg,{state,State}}),

	ets:insert(AgentTab,{Agent,AgentState}),

        {next_state,State,SData};

handle_event(Msg={AgentID,#agentStatus{payload={Pid,started}}},State,
	SData=#cc_state{agent_ptab=AgentPTab,
		     cconf=CC=#controllerConf{numagents=NumAgents},
		     curagents=CurAgents})->

	%% we have a notification from an agent that it has started

	?info({handleEvent,Msg,{state,State}}),

	ets:insert(AgentPTab,{Pid,AgentID}),
	notify({AgentID,Pid,started}),

	{next_state,State,SData};

handle_event(Msg=resume,State,SData=#cc_state{agent_tab=AgentTab,
				       aconf=AgentConf})->

	%% resume all of the suspended agents
	?info({handleEvent,Msg,{state,State}}),
	i_resumeAgents(AgentTab,AgentConf),

        {next_state,State,SData};

handle_event(Msg={stopAgents,Type},State,
        SData=#cc_state{agent_ptab=AgentPTab,
		     curagents=CurAgents})
		when State=='INIT';
		     State=='READY';
		     State=='RUNNING';
		     State=='SUSPENDING';
		     State=='SUSPENDED';
		     State=='COMPLETED'->

	?info({handleEvent,Msg,{state,State}}),

	if CurAgents>0->
		?info({activeAgents,CurAgents}),
		i_stopAgents(AgentPTab,Type),
		notify_listeners({state,'WAIT_STOP'}),
		?info({nextstate,'WAIT_STOP'}),
		timer:send_after(?CC_STOP_TMOUT,{timeout,waitstop}),
		{next_state,'WAIT_STOP',SData};
	true->
		notify_listeners({state,'IDLE'}),
		?info(noAgentsActive),
		?info({nextstate,'IDLE'}),
		{next_state,'IDLE',SData}
	end;

handle_event(Msg,State,SData)->
	?info({unhandledEvent,Msg,{state,State}}),
        {next_state,State,SData}.

handle_info(Msg={'EXIT',Pid,Reason},State,
	    SData=#cc_state{agent_tab=AgentTab,
			 agent_ptab=AgentPTab,
			 curagents=CurAgents})->

	?dbug({handleInfo,Msg,{state,State}}),

	%% We have a linked process exiting. It may be a agent and if so,
	%% we need to remove the entry from the ets table and notify the 
	%% FSM that a agent has exited

	%% look up the ets table to see if the Pid corresponds to a registered
	%% agent

	NewCurAgents=CurAgents-1,

	case ets:lookup(AgentPTab,Pid) of
	[{Pid,AgentID}]->

		%% lookup the agents previous state before exiting

		[{_,PreviousState}]=ets:lookup(AgentTab,AgentID),

		?dbug({agentExit,AgentID,{reason,Reason},{state,State}}),

		%% yes - exited process is an agent, so we delete it from
		%% the process table agent_ptab 

		ets:delete(AgentPTab,Pid),

		%% change the status of the Agent in the table agent_tab

		%% ets:insert(AgentTab,{AgentID,{exit,Reason}}),
		ets:delete(AgentTab,AgentID),

		notify({agent_exit,AgentID,PreviousState,Reason}),
		{next_state,State,SData#cc_state{curagents=NewCurAgents}};

	_->
		?warn({unknownExit,{pid,Pid},{reason,Reason}}),
		{next_state,State,SData}
	end;


handle_info(Msg={timeout,Timeout},State,SData)->
	%% timer event
	?info({handleInfo,Msg}),
	notify(Msg),
	{next_state,State,SData};

handle_info(Msg,State,SData)->
	%% unhandled event
	?info({unhandledInfo,Msg}),
	{next_state,State,SData}.

terminate(Reason,State,_SData)->
	?critical({stopping,Reason,{state,State}}),
        {stop,Reason}.

%% Handling any configuration changes .......

%% check for agent number changes first
%% if controller is any state other than 'IDLE' we should actually adjust the
%% agents to the correct total

handle_controller_config_change(NewCConf=#controllerConf{numagents=N},State,
	SData=#cc_state{cconf=CC=#controllerConf{numagents=M},
		     aconf=AC}) when N>M,State=/='IDLE'->
	%% case 1, increase the number of agents

	?warn({addingAgents,{from,M},{to,N}}),
	i_addAgents(M+1,N-M,AC),

        {reply,ok,State,SData#cc_state{cconf=CC#controllerConf{numagents=N}}};

handle_controller_config_change(NewCConf=#controllerConf{numagents=N},State,
	SData=#cc_state{cconf=CC=#controllerConf{numagents=M}}) when N<M,State=/='IDLE'->
	%% case 2, reduce the number of agents

	?warn({reducingAgents,{from,M},{to,N}}),
	i_removeAgents(M,M-N),
        {reply,ok,State,SData#cc_state{cconf=CC#controllerConf{numagents=N}}};

handle_controller_config_change(NewCConf=#controllerConf{numagents=N},State,
	SData=#cc_state{cconf=CC=#controllerConf{numagents=M}}) when N=/=M-> 
	%% case 3, State='IDLE' so just adjust the number of agents
	%% defined in #controllerCinf
	?warn({adjustingAgentsIDLEState,{from,M},{to,N}}),
        {reply,ok,State,SData#cc_state{cconf=CC#controllerConf{numagents=N}}};

handle_controller_config_change(NewCConf,State,SData)->
	?warn({unhandledConfigChange,NewCConf}),
        {reply,ok,State,SData#cc_state{cconf=NewCConf}}.

handle_agent_config_change(NewAConf,State,SData)->
	?warn({unhandledConfigChange,NewAConf}),
        {reply,ok,State,SData#cc_state{aconf=NewAConf}}.

