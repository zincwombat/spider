-module(cmanager).
-behaviour(gen_server).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

%% This module implements the manager for the datagatherer system

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2
        ]).

-export([start/0,
	 start_link/0,
	 stop/0]).

-export([import/0,		%% import data file
	 importList/1,		%% import list
	 purgeData/0,		%% purge the datasource
	 abort/0,		%% controller:stopAgents()
	 reset/0,
	 getJobConf/0,
	 getAgentConf/0,
	 getControllerConf/0,
	 getCState/0,
	 reloadConfig/0,	%% reload the config file
	 suspend/0,
	 resume/0,
	 status/0,
	 nstatus/0,
	 test/0,
	 test/1,
	 fail/0,
	 run/0]).

-export([
         setImportFile/1,       %% set the import file
         setImportFile/2,       %% set the import file and type
	 setOutputFile/1,	%% set the output file
	 getOutputFile/0,	%% get the output file
	 setFilter/1		%% set the output filter
        ]).


-export([
	 initConf/0,		%% initialise the controller and agent configurations
	 deleteConf/0,		%% delete the configurations
	 setNumAgents/1,	%% set the number of agents
	 setAgentConf/1,	%% set the agent configuration
	 setAgentInvoke/1,	%% set the agent invoke fun
	 setModuleFunction/3,	%% set the Module and Function to be used by Agent
	 setAgentMaxRequests/1,	%% set the allowed number of agent requests
	 setInvokeTimeout/1,	%% set the invoke timeout
	 setJobDescription/1,	%% set the job description
	 setEmail/1		%% set the email address
	]).

-export([cget/1]).		%% get parameters

-export([setDefaultAgentConfig/0,
	 test/0]).		%% used for testing



-include("schema.hrl").
-include("agent.hrl").
-include("filter.hrl").
-include("urn_agent.hrl").
-include("cmanager.hrl").
-include("nstatus.hrl").
-include("regex.hrl").
-sysloglevel(?TRACE_LEVEL).

%% used for testing purposes only

-include("agent_test.hrl").

-define(START_OPTIONS,          []).
-define(SERVERNAME,             cmanager).
-define(SERVERACCESS,           {local,?SERVERNAME}).
-define(SERVERCALL,             ?SERVERNAME).

-define(DEFAULT_RESOLVER,	{127,0,0,1}).
-define(DEFAULT_ALT_RESOLVER,	{203,27,226,130}).

start_link()->
	start().


start()->
        gen_server:start_link(  ?SERVERACCESS,
				?SERVERNAME,
				[],
                                ?START_OPTIONS).

stop()->
        gen_server:call(?SERVERCALL,die).

cget(Param)->
	gen_server:call(?SERVERCALL,{cget,Param}).

fail()->
	gen_server:call(?SERVERCALL,fail).


%% set job parameters ----------------------------------------------------------

initConf()->
	gen_server:call(?SERVERCALL,initConf).

deleteConf()->
	gen_server:call(?SERVERCALL,deleteConf).

setNumAgents(NumAgents) when is_list(NumAgents)->
	setNumAgents(list_to_integer(NumAgents));

setNumAgents(NumAgents) when is_integer(NumAgents)->
	gen_server:call(?SERVERCALL,{setNumAgents,NumAgents});

setNumAgents(NumAgents)->
	{error,badarg}.

getAgentConf()->
	gen_server:call(?SERVERCALL,getAgentConf).

getCState()->
	gen_server:call(?SERVERCALL,getCState).

setAgentConf(AgentConf)->
	gen_server:call(?SERVERCALL,{setAgentConf,AgentConf}).

getControllerConf()->
	gen_server:call(?SERVERCALL,getControllerConf).

setAgentMaxRequests(MaxRequests)->
	gen_server:call(?SERVERCALL,{setAgentMaxRequests,MaxRequests}).

setInvokeTimeout(Timeout)->
	gen_server:call(?SERVERCALL,{setInvokeTimeout,Timeout}).

setJobDescription(Desc)->
	gen_server:call(?SERVERCALL,{setJobDescription,Desc}).

setEmail(Email)->
	gen_server:call(?SERVERCALL,{setEmail,Email}).

setImportFile(File)->
        gen_server:call(?SERVERCALL,{setImportFile,lists:flatten(File)}).

setImportFile(File,Type)->
        gen_server:call(?SERVERCALL,{setImportFile,lists:flatten(File),Type}).

setFilter(Filter)->
        gen_server:call(?SERVERCALL,{setFilter,Filter}).

setOutputFile(File)->
        gen_server:call(?SERVERCALL,{setOutputFile,File}).

getOutputFile()->
        gen_server:call(?SERVERCALL,getOutputFile).

setModuleFunction(Module,Function,AnalysisKey)->
        gen_server:call(?SERVERCALL,{setModuleFunction,Module,Function,AnalysisKey}).

setAgentInvoke(Fun)->
        gen_server:call(?SERVERCALL,{setAgentInvoke,Fun}).


%% set test AgentConf (see: agent_test.hrl) ------------------------------------

setDefaultAgentConfig()->
	setAgentConf(?TESTCONF2).

getJobConf()->
        gen_server:call(?SERVERCALL,getJobConf).

test()->
	test(5).

test(N)->
	reset(),
	import(),
	reset(),
	run().

status()->
	gen_server:call(?SERVERCALL,status).

nstatus()->
	gen_server:call(?SERVERCALL,nstatus).

reset()->
        gen_server:call(?SERVERCALL,reset).

suspend()->
        gen_server:call(?SERVERCALL,suspend).

resume()->
        gen_server:call(?SERVERCALL,resume).

import()->
	gen_server:call(?SERVERCALL,import,?CM_IMPORT_TMOUT).

purgeData()->
	gen_server:call(?SERVERCALL,purgeData).

importList(L)->
	gen_server:call(?SERVERCALL,{importList,L},?CM_IMPORT_TMOUT).

abort()->
	gen_server:call(?SERVERNAME,abort).

run()->
	gen_server:call(?SERVERNAME,run).

reloadConfig()->
	gen_server:call(?SERVERNAME,reloadConfig).

initClookup(Key)->
	case cconfigdb:lookup(Key) of
	undefined->
		Reason={configMissing,Key},
		throw({stop,Reason});
	{ok,Val}->
		Val
	end.

init(_)->
	process_flag(trap_exit,true),

	%% subscribe to configdb events
	configdb_evh:subscribe(),

	%% subscribe to controller events
	controller_evh:subscribe(),

	%% subscribe to datasource events
	datasource_evh:subscribe(),

	regex_controller:compile(?INIT_REGEX_LIST),

	%% read the controller and agent parameters, 

	AgentTimeout=initClookup('DG_AGENT_TIMEOUT'),
	AgentSleep=initClookup('DG_AGENT_SLEEP'),
	AgentMaxReq=initClookup('DG_AGENT_MAXREQUESTS'),
	Logger={LMod,LFun}=initClookup('DG_AGENT_DEFAULT_LOGGER'),
	Getter={GMod,GFun}=initClookup('DG_AGENT_DEFAULT_GETTER'),

	NumAgents=initClookup('DG_AGENT_DEFAULTNUM'),
	MaxAgents=initClookup('DG_AGENT_MAXNUM'),

	?info({numagents,NumAgents}),
	?info({maxagents,MaxAgents}),


	%% read the job parameters

	RlogDir=initClookup('DG_AGENT_DEFAULT_LOGDIR'),
	RlogFile=initClookup('DG_AGENT_DEFAULT_LOGFILE'),

	%% read the config file
	ConfigXML=initClookup('DG_USER_CONFIG_FILE'),

	?info({agentTimeout,AgentTimeout}),
	?info({agentSleep,AgentSleep}),
	?info({agentMaxReq,AgentMaxReq}),
	?info({agentLogger,Logger}),
	?info({agentGetter,Getter}),
	?info({configXML,ConfigXML}),

	%% read the resolvers
	NS=initClookup('DNS_RES_NS'),
	ALT_NS=initClookup('DNS_RES_ALT_NS'),

	?info({resolvers,NS,ALT_NS}),

	%% configure inet_db with the resolvers

	inet_db:del_ns(),
	inet_db:del_alt_ns(),
	inet_db:add_ns(NS),
	inet_db:add_alt_ns(ALT_NS),

	%% create the template config records from the default values

	ACT=#agentConf{
		timeout=AgentTimeout,
               	sleep=AgentSleep,
               	max_rq=AgentMaxReq,
		get=fun(Z)->apply(GMod,GFun,[Z]) end,
		logger=fun(Z)->apply(LMod,LFun,[Z]) end},

	CCT=#controllerConf{
		numagents=NumAgents,
               	maxagents=MaxAgents,
		res_ns=NS,
		res_alt_ns=ALT_NS},

	JCT=#jobConf{
		logfile=RlogFile,
		logdir=RlogDir},
	
	{ok,#cm_state{cstate='IDLE',
                         configfile=ConfigXML,
                         def_agentConf=ACT,
                         agentConf=ACT,
                         def_controllerConf=CCT,
                         controllerConf=CCT,
                         def_jobConf=JCT,
                         jobConf=JCT
			}}.

readConfig(State=#cm_state{configfile=ConfigXML,
			agentConf=ACT,
			controllerConf=CCT,
			jobConf=JCT})->

	case readConfig(ConfigXML,{ACT,CCT,JCT}) of
	E={error,Reason}->
		State;

	X={ACT,CCT,JCT}->
		?info({noChangeInConfig,X}),
		State;

	X={AC,CC,JC}->
		%% need to add a hook here for config changes
		%% before -> after
		?info({newConfig,X}),
		State#cm_state{agentConf=AC,
			    controllerConf=CC,
			    jobConf=JC}
	end.

readConfig(ConfigXML,{ACT,CCT,JCT})->
	case (catch xmlconfig:parse(ConfigXML,{ACT,CCT,JCT})) of
        E={error,Reason}->
                ?critical({xmlConfigError,E}),
                E;

        Reply={AC,CC,JC}->
                ?info({xmlconfig,Reply}),
		Reply
        end.
		

handle_call(die,_,State)->
        {stop,normal,State};


handle_call(Msg=reloadConfig,_,State=#cm_state{cstate=CSTATE})->
	case CSTATE of
	'IDLE'->
		?info(reloadConfig),
		case readConfig(State) of
		State->
			?info(noStateChange),
			{reply,nochange,State};

		NewState=#cm_state{controllerConf=CC,
				   agentConf=AC}->
			?info(stateChanged),
			controller:setControllerConf(CC),
			controller:setAgentConf(AC),
			{reply,ok,NewState}
		end;

	Other->
		?warn({reloadConfigWrongState,Other}),
		{reply,ignored,State}
	end;

handle_call(initConf,_,State=#cm_state{
					cstate='IDLE',
					def_agentConf=AC,
					def_jobConf=JC,
					def_controllerConf=CC
				})->

	?info({settingInitialConfigurations,AC,JC,CC}),

	controller:setAgentConf(AC),
	controller:setControllerConf(CC),

	{reply,ok,State#cm_state{agentConf=AC,
				 controllerConf=CC,
				 jobConf=JC}};

handle_call(deleteConf,_,State=#cm_state{cstate='IDLE'})->

	?info(deletingInitialConfigurations),

	controller:setAgentConf(#agentConf{}),
	controller:setControllerConf(#controllerConf{}),

	{reply,ok,State#cm_state{agentConf=#agentConf{},
				 jobConf=#jobConf{},
				 controllerConf=#controllerConf{}}};

handle_call({setNumAgents,NumAgents},_,
	    State=#cm_state{controllerConf=CC=#controllerConf{numagents=N,
					    maxagents=Max}}) when NumAgents=<Max->

	?info({numAgentsChange,{from,N},{to,NumAgents},{max,Max}}),
	NewCC=CC#controllerConf{numagents=NumAgents},
	Reply=controller:setControllerConf(NewCC),
	{reply,Reply,State#cm_state{controllerConf=NewCC}};



handle_call({setNumAgents,NumAgents},_,
	    State=#cm_state{controllerConf=CC=#controllerConf{numagents=N,
					    maxagents=Max}})->

	?warn({limitExceeded,{requested,NumAgents},{maxagents,Max}}),
	{reply,{limitExceeded,{maxagents,Max}},State};

handle_call(Msg={setAgentConf,AgentConf},_,State)
	 when record(AgentConf,agentConf)->
	?info(Msg),
	Reply=controller:setAgentConf(AgentConf),
	{reply,Reply,State#cm_state{agentConf=AgentConf}};

handle_call(Msg=getAgentConf,_,State)->
	Reply=controller:getAgentConf(),
	{reply,Reply,State};

handle_call(Msg=getControllerConf,_,State)->
	Reply=controller:getControllerConf(),
	{reply,Reply,State};

handle_call(Msg={setAgentMaxRequests,_MaxRequests},_,State)->
	?info({unhandledCall,Msg}),
	{reply,ignored,State};

handle_call(Msg={setInvokeTimeout,_Timeout},_,State)->
	?info({unhandledCall,Msg}),
	{reply,ignored,State};


handle_call(Msg={setJobDescription,JobDescription},_,
	State=#cm_state{jobConf=JC})->
	?info({settingJobDescription,JobDescription}),
	{reply,ok,State#cm_state{jobConf=JC#jobConf{jobdesc=JobDescription}}};

handle_call(Msg={setEmail,Email},_,
	State=#cm_state{jobConf=JC})->
	?info({settingEmail,Email}),
	{reply,ok,State#cm_state{jobConf=JC#jobConf{email=Email}}};

handle_call(Msg=getJobConf,_,
	State=#cm_state{jobConf=JC})->
	{reply,{ok,JC},State};

handle_call(Msg=status,_,State=#cm_state{cstate=CState,
				      datasource=DataSource})->
	?dbug({handleCall,Msg}),
	CStatus=controller:status(),
	{reply,{ok,{{manager,State},{controller,CStatus}}},State};

handle_call(Msg=getCState,_,State=#cm_state{cstate=CState})->
	?dbug({handleCall,Msg}),
	{reply,{ok,{cstate,CState}},State};

handle_call(Msg=nstatus,_,State)->
	?dbug({handleCall,Msg}),
	NStatus=#nstatus{cm_state=State,
			 ds_state=datasource:nstatus(),
			 cc_state=controller:nstatus()},
	
        {reply,objectMapper:map(NStatus),State};

handle_call(Msg=suspend,_,State)->
	?dbug({handleCall,Msg}),
	Reply=controller:suspend(),
	{reply,Reply,State};

handle_call(Msg=resume,_,State)->
	?dbug({handleCall,Msg}),
	Reply=controller:resume(),
	{reply,Reply,State};

handle_call(Msg=abort,_,State=#cm_state{cstate=CState})->
	?info({handleCall,Msg,{state,CState}}),
	case CState of
	'IDLE'->
		{reply,ignored,State};
	_Other->
		?info(stoppingAgents),
		Reply=controller:stopAgents(),
        	{reply,Reply,State,?CM_ABORT_TMOUT}
	end;

handle_call(Msg=run,_,State=#cm_state{cstate='IDLE',
				   datasource=ready,
				   agentConf=AConf=#agentConf{invoke=I,
							      get=G,
							      logger=L},
				   controllerConf=CConf,
				   jobConf=JC=#jobConf{logdir=Dir,
						  logfile=File}}) when is_function(I),
								       is_function(G),
								       is_function(L)->

	%% this is the correct state in which to start a run
	%% we just need to ensure that AConf is valid TBD!
	%% note: we require that the datasource be initialised
	%% before we can run

	?info({handleCall,Msg}),

	%% close any currently open logfiles, and open a new one

	rlogger:reset(Dir,File),

	%% write the start message
	%% arguably this is redundant .....

	rlogger:write(startJob),

	%% write the job control element
	rlogger:write(JC),

	%% write the controller element
	rlogger:write(CConf),
	controller:initAgents(AConf,CConf),

	{reply,ok,State};

handle_call(Msg=run,_,State=#cm_state{cstate='IDLE',
                                   datasource=ready,
                                   agentConf=AConf})->
	case AConf of
		#agentConf{invoke=I} when not is_function(I)->
			{reply,{error,{agentConf,bad_invoke,I}},State};
		#agentConf{get=G} when not is_function(G)->
			{reply,{error,{agentConf,bad_getter,G}},State};
		#agentConf{logger=L} when not is_function(L)->
			{reply,{error,{agentConf,bad_logger,L}},State}
	end;

handle_call(Msg=run,_,State=#cm_state{cstate='IDLE',
                                      datasource=DS_State}) when DS_State/=ready->
	%% datasource has not been setup
	?warn({handleCall,Msg}),
	{reply,{error,{datasource,DS_State}},State};

handle_call(Msg=run,_,State)->
	?info({handleCall,Msg,{state,State},ignored}),
	{reply,{error,{wrong_state,State}},State};

handle_call(Msg=reset,_,State=#cm_state{cstate=_CState})->
	?info({handleCall,Msg}),

	CReply=controller:stopAgents(),
	DReply=datasource:reset(),

	Reply=
	case [CReply,DReply] of
	[ok,ok]->
		ok;

	Other->
		{error,Other}
	end,

       	{reply,Reply,State};

handle_call({importList,L},_,State=#cm_state{jobConf=JC,cstate='IDLE'}) when is_list(L)->
	?info({importListRequest,L}),
	Import=#import{type=list,value=L},
	Reply=datasource:import(Import),
	{reply,Reply,State#cm_state{jobConf=JC#jobConf{importSpec=Import#import{value={length,length(L)}}}}};

handle_call(import,_,
	    State=#cm_state{jobConf=#jobConf{importSpec=Import},cstate='IDLE'})->
	?info({importRequest,Import}),
	Reply=datasource:import(Import),
	{reply,Reply,State};

handle_call(fail,_,State)->
	%% trigger a fail .....
	Reply=
	try
		foo:bar()
	catch
		exit:Reason->{exit,Reason};
		error:Reason->{caught,{error,Reason}};
		Other->Other
	end,
	{reply,Reply,State};

handle_call(purgeData,_,
	    State=#cm_state{jobConf=JC=#jobConf{importSpec=Import},cstate='IDLE'})->
	?info(purgeData),
	Reply=datasource:purge(),
	{reply,Reply,State#cm_state{jobConf=JC#jobConf{importSpec=undefined}}};

handle_call({cget,Param},_,State)->
	?dbug({cget,Param}),
	handle_get(Param,State);

handle_call({setFilter,Filter},_,State=#cm_state{jobConf=JC=#jobConf{filters=F}}) when F=/=[]->
	?info({setFilter,Filter}),

	%% check that the specified filter is valid

	Reply=
	case isFilter(Filter,JC) of
	true->
		Module=JC#jobConf.moduleid,
		case (catch rlogger:addFilter({Module,Filter})) of
		ok->
			{reply,ok,State#cm_state{jobConf=JC#jobConf{filter=Filter}}};

		Other->
			?warn({filterError,Other}),
			{reply,Other,State}
		end;
	_->
		?warn({unknownFilter,Filter}),
		{reply,{error,{unknownFilter,Filter}},State}
	end;

handle_call({setImportFile,FileName},_,State)->
        handle_setImportFile(FileName,"csv",State);

handle_call({setImportFile,FileName,FileType},_,State)->
        handle_setImportFile(FileName,FileType,State);

handle_call({setAgentInvoke,Fun},_,State) when is_function(Fun)->
	?dbug({setAgentInvoke,Fun}),
	case (catch controller:getAgentConf()) of
	{ok,AgentConf=#agentConf{invoke=I}}->
		controller:setAgentConf(AgentConf#agentConf{invoke=Fun}),
		{ok,NewAgentConf}=controller:getAgentConf(),
        	{reply,ok,State#cm_state{agentConf=NewAgentConf}};

	Other->
		?warn({setInvoke,Other}),
		{reply,{error,Other},State}
	end;

handle_call({setModuleFunction,Module,Function,AnalysisKey},_,
	    State=#cm_state{cstate='IDLE',
			    jobConf=JC=#jobConf{}})->
							

	%% need to update jobDesc and then
	%% update the agent configuration
	%% only applicable in state 'IDLE'

	?info({setModuleFunction,{Module,Function,AnalysisKey}}),

	Invoke=fun(Z)->apply(Module,Function,[Z]) end,

	Filters=
	case (catch apply(Module,getFilters,[])) of
	F=[F1|_]->
		F;
	E->
		?warn({noFilters,E}),
		[]
	end,

	case (catch controller:getAgentConf()) of
	{ok,AgentConf=#agentConf{invoke=I}}->
		controller:setAgentConf(AgentConf#agentConf{invoke=Invoke}),
		{ok,NewAgentConf}=controller:getAgentConf(),
        	{reply,ok,State#cm_state{agentConf=NewAgentConf,
					 jobConf=JC#jobConf{moduleid=Module,
							    functionid=Function,
							    analysis_key=AnalysisKey,
							    filters=Filters}}};

	Other->
		?warn({setInvoke,Other}),
		{reply,{error,Other},State}
	end;

handle_call({setOutputFile,FileName},_,State)->
        handle_setOutputFile(FileName,State);

handle_call(getOutputFile,_,State=#cm_state{jobConf=#jobConf{logdir=D,logfile=F}})->
        {reply,{ok,F},State};
                                                                                
handle_call(Msg,_,State) ->
	%% unhandled message
	?info({unhandledCall,Msg,{state,State}}),
        {reply,ignored,State}.

handle_cast(Msg,State)->
	%% unhandled message
	?info({unhandledCast,Msg}),
        {noreply,State}.

%% the following defines the business process, based on the handling
%% of events received from the controller_evh and datasource_evh
%% handlers

handle_info(Msg={event,{controller,{state,CState='READY'}}},State)->
        ?dbug({handleInfo,Msg}),
        ?dbug(startRun),
	controller:run(),
        {noreply,State#cm_state{cstate=CState}};

handle_info(Msg={event,{controller,{state,CState='RUNNING'}}},State)->
        ?dbug({handleInfo,Msg}),
        {noreply,State#cm_state{cstate=CState}};

handle_info(Msg={event,{controller,{state,CState='COMPLETED'}}},State)->
        ?dbug({handleInfo,Msg}),
	%% TBD we need to get the stop_tm off the controller
	rlogger:write(endJob),
	rlogger:close(),
        {noreply,State#cm_state{cstate=CState}};

handle_info(Msg={event,{controller,{state,CState='IDLE'}}},State=#cm_state{cstate='WAIT_STOP'})->
	%% this results from a requested STOP so we need to write the endJob token
        ?dbug({handleInfo,Msg}),
	%% TBD we need to get the stop_tm off the controller
	rlogger:write(endJob),
	rlogger:close(),
        {noreply,State#cm_state{cstate=CState}};

handle_info(Msg={event,{controller,{state,CState}}},State)->
        ?dbug({handleInfo,Msg}),
        {noreply,State#cm_state{cstate=CState}};

handle_info(Msg={event,{datasource,{import,{source,_Source},{size,_Size},{status,_Status}}}},State)->
        ?dbug({handleInfo,Msg}),
	%% we dont want the run to start automatically
        {noreply,State#cm_state{datasource=ready}};

handle_info(Msg={event,{datasource,{import,{error,_Reason}}}},State)->
        ?warn({handleInfo,Msg}),
        {noreply,State#cm_state{datasource=undefined}};

handle_info(Msg={event,{datasource,reset}},State)->
        ?dbug({handleInfo,Msg}),
        {noreply,State};

handle_info(Msg={event,{datasource,purged}},State=#cm_state{cstate=CState})->
        ?dbug({handleInfo,Msg}),
	case CState of
	'RUNNING'->
		?info(stoppingController),
		controller:stop();
	_->
		ignore
	end,
        {noreply,State#cm_state{datasource=empty}};

handle_info(Msg={event,_Event},State)->
	?dbug({ignoringEvent,Msg}),
        {noreply,State};

handle_info(Msg,State)->
        ?info({unhandledInfo,Msg}),
        {noreply,State}.

terminate(Reason,_State) ->
	?critical({stopping,Reason}),
        ok.

handle_get(G={datasource,source},State=#cm_state{jobConf=#jobConf{importSpec=IS}})->
	?dbug({get,G}),
	{reply,{ok,IS},State};

handle_get(G,State)->
	?warn({unhandledGet,G}),
	{reply,{error,undefined},State}.

handle_setImportFile(FileName,FileType,State=#cm_state{jobConf=JC})->
        ?warn({setImportFile,FileName,FileType}),

	%% added TOH 21/6/2014 -- if no directory part in the FileName, use the predefined
	%% upload directory

	BaseDir=initClookup('DG_DATASOURCE_DEFAULT_UPLOAD_DIR'),
	FullPathFileName=
	case string:chr(FileName,$/) of
	0->
		?warn({no_dir,FileName}),
		BaseDir ++ "/" ++ FileName;

	_->
		FileName
	end,

	?info({importFileName,FullPathFileName}),
        {reply,ok,State#cm_state{jobConf=JC#jobConf{importSpec=#import{type=file,
								   format=FileType,
								   value=FullPathFileName}}}}.

handle_setOutputFile(FileName,State=#cm_state{jobConf=JC})->
        ?dbug({setOutputFile,FileName}),
        {reply,ok,State#cm_state{jobConf=JC#jobConf{logfile=FileName}}}.

isFilter(Key,JC=#jobConf{filters=Fs=[F|_]})->
	isFilter(Key,Fs);

isFilter(Key,[])->
	false;

isFilter(Key,[#filter_opt{key=Key}|_])->
	true;

isFilter(Key,[F|Rest])->
	isFilter(Key,Rest).
	
