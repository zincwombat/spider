-module(agent).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").
-behaviour(gen_fsm).



-export([       
	init/1,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3
]).


-export([
        start/1,	%% start the FSM
        start/2,	%% start the FSM
        start/3,	%% start the FSM
        stop/1,		%% stop the FSM
	state/1,	%% return the FSM current state
	reset/1,	%% set the state to 'IDLE'
	message/2,	%% general message sending interface
	a_message/2,	%% general message sending interface
	run/1,		%% start the agent
	suspend/1,	%% suspend the agent
	resume/1,	%% resumethe agent
	i_invoke/3	%% the function that does the work!
]).

-export([
	'IDLE'/2,	%% Agent has been initialised and is ready 
	'SUSPENDED'/2,	%% Agent has been suspended by the controller
	'FINISHED'/2,	%% Agent has no more work to do
	'WAIT_DATA'/2,	%% Agent is waiting for data from the controller
	'WAIT_EXEC'/2	%% Agent is operational
	]).

-define(START_OPTIONS,  []).
-define(SERVERNAME,     ?MODULE).

%% GLOBAL DEFINES =============================================================

-define(SYNC_TMOUT,	60000).	%% maximum time allowed waiting for a
				%% SYNC request
-define(DEFAULT_TMOUT,	60000).	%% default timeout

-include("agent.hrl").
-include("urn_agent.hrl").

%% STATE DATA =================================================================

-record(state,{		
		replyTo,	%% PID or fun to send the reply to
		agentID,	%% agentID, set by controller
		aconf,	%% request parameters
		counter=0,	%% number of requests processed
		data,		%% current request data
		spid,		%% PID of spawned process
		suspend_req	%% requested suspend
}).	

%% UTILITY FUNCTIONS ===========================================================

resetState(SD=#state{})->
	%% reset the state variable
	SD#state{counter=0,
		 data=undefined,
		 spid=undefined,
		 suspend_req=false}.

callback(AgentID,Tag)->
	%% this is a callback function passed to spawned processes
	%% it is encapsulated in a fun and provides a Tag to allow 
	%% matching of request/response pairs. In order to use the
	%% callback, the target process evaluates the fun with a 
	%% reply, e.g. callback(Reply)
	fun(Z)->catch(gen_fsm:send_event(AgentID,{Tag,Z})) end.

%% PUBLIC API

reset(AgentID)->
	%% set the FSM state to 'IDLE'
	gen_fsm:sync_send_all_state_event(AgentID,reset).

message(AgentID,Message)->
	%% asynchronous interface used to send a message to an agent
	gen_fsm:send_event(AgentID,Message).

a_message(AgentID,Message)->
	%% asynchronous interface used to send a message to an agent
	gen_fsm:send_all_state_event(AgentID,Message).

run(AgentID)->
	gen_fsm:send_event(AgentID,{control,run}).

suspend(AgentID)->
	gen_fsm:send_event(AgentID,{control,suspend}).

resume(AgentID)->
	gen_fsm:send_event(AgentID,{control,resume}).

sync(AgentID,Msg,Timeout)->
        gen_fsm:sync_send_all_state_event(AgentID,Msg,Timeout).

state(AgentID)->
	%% return the current FSM state to the calling process
	gen_fsm:sync_send_all_state_event(AgentID,state).



%% REPLY HANDLING ==============================================================

%% LOGGER INTERFACE

%% the FSM sends the results to the logger using the log/1 function, behaviour 
%% is dependent upon whether ReplyTo is a Pid or a Fun. All results from the
%% agent must be encapsulated into the #agentResult{} record which is defined
%% in agent.hrl

log(Msg,#state{agentID=AgentID,aconf=#agentConf{logger=Logger}})->
	%% first construct the #agentResult{} record,
	AgentResult=#agentResult{agentID=AgentID,
				 timestamp=erlang:now(),
				 payload=Msg},
	log(AgentResult,Logger);

log(Reply,ReplyTo) when is_pid(ReplyTo)->
	%% ReplyTo is a Pid, so send the message directly to the
	%% process
	ReplyTo ! Reply;

log(Reply,ReplyTo) when is_function(ReplyTo)->
	%% ReplyTo is a Fun/1, so invoke the Fun
	%% need to wrap this in a catch TBD
	?dbug({logging,{fn,ReplyTo},{reply,Reply}}),
	ReplyTo(Reply).

%% CONTROLLER INTERFACE

%% the FSM sends messages to the controller using the notify/1 function
%% behaviour is dependent upon whether ReplyTo is a Pid or a Fun. 
%% All messages from the agent must be encapsulated into the #agentStatus{} 
%% record which is defined in agent.hrl

notify(Msg,#state{agentID=AgentID,replyTo=ReplyTo})->
	%% first construct the #agentStatus{} record,
	AgentStatus=#agentStatus{agentID=AgentID,
				 timestamp=erlang:now(),
				 payload=Msg},
	notify(AgentStatus,ReplyTo);

notify(Status,ReplyTo) when is_pid(ReplyTo)->
	%% ReplyTo is a Pid, so send the message directly to the
	%% process
	ReplyTo ! Status;

notify(Status,ReplyTo) when is_function(ReplyTo)->
	%% ReplyTo is a Fun/1, so invoke the Fun
	ReplyTo(Status).

%% START/INIT ==================================================================

start(AgentID)->
	%% called by the controlling process (e.g. dispatcher)
	%% the FSM initialises into the 'IDLE' state, and the FSM sets an
	%% address to AgentID. 
        gen_fsm:start_link({local,AgentID},?MODULE,[{AgentID,self()},[]],[]).

start(AgentID,CallBack) when is_function(CallBack)->
	%% as above, but this time a callback function is defined and the
	%% FSM must invoke the callback when the result has been returned
	%% the callback is a fun of arity 1, invoked by CallBack(Result)
        gen_fsm:start_link({local,AgentID},?MODULE,[{AgentID,CallBack,[]}],[]).

start(AgentID,CallBack,AR=#agentConf{}) when is_function(CallBack)->
	%% as above, but this time a callback function and the agentConf
	%% is defined and the
	%% FSM must invoke the callback when the result has been returned
	%% the callback is a fun of arity 1, invoked by CallBack(Result)
        gen_fsm:start_link({local,AgentID},?MODULE,[{AgentID,CallBack,AR}],[]).

stop(AgentID)->
	%% stop the FSM, default reason is normal
        stop(AgentID,normal).

stop(AgentID,Reason)->
	%% stop the FSM, reason is defined
	gen_fsm:send_all_state_event(AgentID,{stop,Reason}).

init([{AgentID,ReplyTo,AgentConf}])->

	%% trap exits from linked processes
	%% TBD, need to linked spawned processes as well!

        process_flag(trap_exit,true),
	%% notify controller that we have started, send the AgentID and PID

	State=#state{agentID=AgentID,replyTo=ReplyTo,aconf=AgentConf},

	%% notify the controller that we have started

	notify({state,{'IDLE',{init,self()}}},State),

	%% transit to the IDLE state

	%% store the agentID in the process dictionary for easy 
	%% retrieval

	put(agentId,AgentID),

	?info({started,AgentID,self()}),

	{ok,'IDLE',State}.

%% STATE TIMEOUTS ==============================================================

timeout(_)->
	?DEFAULT_TMOUT.

%% INVOKE handling =============================================================

%% we need to spawn a process to invoke the fun provided in the AgentConf
%% this invoke will be supervised using a timeout provided in the Request.

i_invoke(#agentConf{invoke=null},_Data,CallBack)->
	%% null invoke, used for performance testing, for example
	Reply=null_invoke,
	CallBack(Reply),
	exit(normal);

i_invoke(#agentConf{invoke=Invoke},Data,CallBack) when is_function(Invoke)->
	%% we need to check that the result returned here is not an error
	Reply=(catch Invoke(Data)),
	CallBack(Reply),
	exit(normal).

%% STATE CALLBACKS =============================================================



%% IDLE STATE ------------------------------------------------------------------ 

-undef(THIS_STATE).
-define(THIS_AGENT,{agent,get(agentId)}).
-define(THIS_STATE,'IDLE').

'IDLE'(Msg={control,AgentConf=#agentConf{}},SData=#state{agentID=A})->

	%% request parameters are loaded into the #agentConf record

	?dbug({?THIS_AGENT,?THIS_STATE,Msg}),

	{next_state,'IDLE',SData#state{aconf=AgentConf}};

'IDLE'(Msg={control,run},SData=#state{agentID=A,
				      aconf=#agentConf{get=Get}})->

	%% the controller has requested that the Agent start. All the
	%% request parameters are loaded into the #agentConf record

	?dbug({?THIS_AGENT,?THIS_STATE,Msg}),


	%% AgentConf contains the invoke/1 fun, which we must
	%% spawn here and then transit to the 'WAIT_EXEC' state.
	%% first we must get the Data from the controller function

	%% asynchronous request to get some data, we supply a callback
	%% fun as an argument

	Get(fun(Z)->gen_fsm:send_event(A,Z) end),

	notify({state,'RUNNING'},SData),

	%% jump to state 'WAIT_DATA' TBD add a timeout here!

	?dbug({{agent,A},nextstate,'WAIT_DATA'}),

	{next_state,'WAIT_DATA',SData};

'IDLE'(Msg={control,stop},SData=#state{agentID=A})->

	%% controller requests stop

	?dbug({?THIS_AGENT,?THIS_STATE,Msg}),

	{stop,normal,SData};

'IDLE'(Msg={control,suspend},SData=#state{agentID=A})->

	%% controller requests suspend
	?dbug({?THIS_AGENT,?THIS_STATE,Msg}),

	notify({state,'SUSPENDED'},SData),
	{next_state,'SUSPENDED',SData};

'IDLE'(Msg,SData=#state{agentID=A})->
	?dbug({unhandled,?THIS_AGENT,?THIS_STATE,Msg}),
	{next_state,'IDLE',SData}.

%% WAIT_DATA STATE -------------------------------------------------------------

-undef(THIS_STATE).
-define(THIS_STATE,'WAIT_DATA').

'WAIT_DATA'(Msg={data,Data},
	    SData=#state{agentID=A,
			 counter=Counter,
			 aconf=AR=#agentConf{timeout=Timeout}})->
	
	%% we have received data to analyse, spawn off the process
	%% to analyse the data. TBD should we use spawn_link here??
	%% note that we set a state timeout from the agentConf{} record
	%% to supervise the spawned process
	
	?dbug({?THIS_AGENT,?THIS_STATE,Msg}),

	SPid=spawn(?MODULE,i_invoke,[AR,Data,callback(A,{exec,Data})]),

	{next_state,'WAIT_EXEC',SData#state{spid=SPid,
                                            counter=Counter+1,
                                            data=Data},Timeout};

'WAIT_DATA'(Msg={status,finished},SData=#state{agentID=A})->
	
	%% The agent has received the message that means there
	%% is no more data to analyse, so go back to IDLE state
	
	?dbug({?THIS_AGENT,?THIS_STATE,Msg}),

	%% notify the controller that we have finished

	notify({state,'FINISHED'},SData),

	{next_state,'FINISHED',SData};

'WAIT_DATA'(Msg={control,suspend},SData=#state{agentID=A})->
	

	%% suspend request received

	?dbug({?THIS_AGENT,?THIS_STATE,Msg}),

	%% notify the controller that we have finished

	{next_state,'WAIT_DATA',SData#state{suspend_req=true}};


'WAIT_DATA'(Msg,SData=#state{agentID=A})->
	?dbug({unhandled,?THIS_AGENT,?THIS_STATE,Msg}),
	{next_state,'WAIT_DATA',SData}.


%% WAIT_EXEC STATE -------------------------------------------------------------

-undef(THIS_STATE).
-define(THIS_STATE,'WAIT_EXEC').

'WAIT_EXEC'(Msg=timeout,SData=#state{agentID=A,
				     spid=SPid,
				     data=Data,
				     aconf=#agentConf{get=Get},
				     suspend_req=SuspendReq})->

	%% we have timedout while waiting for a response from the 
	%% spawn to i_invoke. The timeout is set in the #agentConf
	%% record. On timeout, we must kill the spawned process

	?dbug({?THIS_AGENT,?THIS_STATE,Msg}),

	Result={systimeout,{data,Data},{limit,timeout(?THIS_STATE)}},

	?dbug({timeout,SPid,{data,Data},{limit,timeout(?THIS_STATE)}}),

	%% note that "Data" is the argument passed to the agent's invoke
	%% function so we should pass this back in the logger response

	log(Result,SData),
	exit(SPid,die),

	if SuspendReq==true->
		notify({state,'SUSPENDED'},SData),
        	{next_state,'SUSPENDED',SData};
	true->
		Get(fun(Z)->gen_fsm:send_event(A,Z) end),
		{next_state,'WAIT_DATA',SData#state{spid=undefined}}
	end;
		

'WAIT_EXEC'(Msg={{exec,_Data},Result},
    	    SData=#state{agentID=A,
			 counter=Counter,
			 aconf=#agentConf{max_rq=Counter}})->

	%% we have reached the defined maximum number of requests, next
	%% state will be 'IDLE'

	?dbug({?THIS_AGENT,?THIS_STATE,Msg}),
	log(Result,SData),

	?info({requestLimitReached,Counter}),

	notify({state,'FINISHED'},SData),
        {next_state,'FINISHED',SData#state{spid=undefined}};

'WAIT_EXEC'(Msg={{exec,_Data},Result},
	    SData=#state{agentID=A,
		 	 aconf=#agentConf{get=Get},
			 suspend_req=SuspendReq})->

	?dbug({?THIS_AGENT,?THIS_STATE,Msg}),

	log(Result,SData),

	if SuspendReq==true->
		notify({state,'SUSPENDED'},SData),
        	{next_state,'SUSPENDED',SData#state{spid=undefined}};
	true->
		Get(fun(Z)->gen_fsm:send_event(A,Z) end),
		{next_state,'WAIT_DATA',SData#state{spid=undefined}}
	end;


'WAIT_EXEC'(Msg={control,suspend},
	    SData=#state{agentID=A,
			 aconf=#agentConf{}})->

	?dbug({?THIS_AGENT,?THIS_STATE,Msg}),

        {next_state,'WAIT_EXEC',SData#state{suspend_req=true}};
	

'WAIT_EXEC'(Msg,SData=#state{agentID=A})->
	%% unhandled message, ignore
	?warn({unhandled,?THIS_AGENT,?THIS_STATE,Msg}),
	{next_state,'WAIT_EXEC',SData}.

-undef(THIS_STATE).
-define(THIS_STATE,'SUSPENDED').

'SUSPENDED'(Msg={control,resume},SData=#state{agentID=A,
				      aconf=#agentConf{get=Get}})->
	?dbug({?THIS_AGENT,?THIS_STATE,Msg}),
	Get(fun(Z)->gen_fsm:send_event(A,Z) end),
	notify({state,'RUNNING'},SData),
	{next_state,'WAIT_DATA',SData#state{spid=undefined,suspend_req=false}};

'SUSPENDED'(Msg,SData=#state{agentID=A})->
	?dbug({unhandled,?THIS_AGENT,?THIS_STATE,Msg}),
	{next_state,'SUSPENDED',SData}.

-undef(THIS_STATE).
-define(THIS_STATE,'FINISHED').

'FINISHED'(Msg,SData=#state{agentID=A})->
	%% unhandled message, ignore
	?dbug({unhandled,?THIS_AGENT,?THIS_STATE,Msg}),
	{next_state,'FINISHED',SData}.


%% END STATE CALLBACKS =========================================================

handle_sync_event(Event={stop,Reason},_From,StateName,SData=#state{agentID=A})->
	?dbug({syncEvent,?THIS_AGENT,{state,StateName},Event}),
        {stop,Reason,SData};

handle_sync_event(Event=state,_From,StateName,SData=#state{agentID=A})->
	?dbug({syncEvent,?THIS_AGENT,{state,StateName},Event}),
        {reply,{StateName,SData},StateName,SData};

handle_sync_event(Event=reset,_From,StateName,
		  SData=#state{agentID=A,spid=SPid}) when is_pid(SPid)->

	?dbug({syncEvent,?THIS_AGENT,{state,StateName},Event}),
	exit(SPid,kill),

	notify({state,'IDLE'},SData),
        {reply,{StateName,'IDLE'},'IDLE',resetState(SData)};

handle_sync_event(Event=reset,_From,StateName,
		  SData=#state{agentID=A})->

	?dbug({syncEvent,?THIS_AGENT,{state,StateName},Event}),
	
	notify({state,'IDLE'},SData),
        {reply,{StateName,'IDLE'},'IDLE',resetState(SData)};

handle_sync_event(Event,_From,StateName,SData=#state{agentID=A})->
	?dbug({syncEvent,?THIS_AGENT,{state,StateName},Event}),
        {StateName,SData,timeout(StateName)}.

handle_event(Event={stop,Reason},StateName,SData=#state{agentID=A})->
	?dbug({event,?THIS_AGENT,{state,StateName},Event}),
        {stop,Reason,SData};

handle_event(Event,StateName,SData=#state{agentID=A})->
	?dbug({event,?THIS_AGENT,{state,StateName},Event}),
        {next_state,StateName,SData,timeout(StateName)}.

handle_info(Msg,StateName,SData=#state{agentID=A})->
	?dbug({info,?THIS_AGENT,{state,StateName},Msg}),
	{next_state,StateName,SData,timeout(StateName)}.

terminate(Reason,StateName,_SData)->
	?dbug({terminate,?THIS_AGENT,{state,StateName},Reason}),
        {stop,Reason}.

