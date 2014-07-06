-define(AGENT_V1_URI,		'urn:agent_v1').
-define(CONTROLLER_V1_URI,	'urn:controller_v1').
-define(JOB_V1_URI,		'urn:job_v1').

-include("embedded.hrl").

-undef(THIS_AGENT_VERSION).
-undef(THIS_CONTOLLER_VERSION).

-define(THIS_AGENT_VERSION,   		?AGENT_V1_URI).
-define(THIS_CONTROLLER_VERSION,	?CONTROLLER_V1_URI).
-define(THIS_JOB_VERSION,		?JOB_V1_URI).

-define(DEFAGENTS,	10).		%% default number of agents
-define(MAXAGENTS,	40).		%% maximum allowed agents
-define(DEFAGENTTMOUT,	30000).		%% default agent timeout (mSecs)
-define(DEFAGENTSLEEP,	0).		%% default agent sleep (mSecs)
-define(DEFAGENTMAXRQ,	unlimited).	%% default agent max requests
-define(DEFLOGGER,	{rlogger_evh,notify}).
-define(DEFGETTER,	{datasource,dget_a}).
-define(DEFCONFIGXML,	"/home/thobbins/dg.xml").
-define(DEFRLOGDIR,	?YAWS_DOCROOT++"/analysis").
-define(DEFRLOGFILE,	"results.log").

-record(agentConf,{
                        invoke,                 %% fun/1 that does the work
                        get,                    %% fun/1 that gets data
                        logger,                 %% fun/1 that logs the data
                        timeout,                %% max time to wait for invoke
                        sleep,                  %% sleep in mS between invokes
                        max_rq                  %% max allowed agent requests
                        }).


-record(agentResult,{	version=?THIS_AGENT_VERSION,
                        agentID,
                        timestamp,
                        payload
			}).

-record(agentStatus,{	version=?THIS_AGENT_VERSION,
                        agentID,
                        timestamp,
                        payload
			}).

-record(controllerConf,{
			version=?THIS_CONTROLLER_VERSION,
			numagents,	%% number of agents,
			maxagents,	%% maximum allowed agents,
			res_ns,
			res_alt_ns
			}).

-record(import,		{
			type,	%% file|list
			format,	%% "erl","ets","csv" etc
			value	%% filename or undefined
			}).

-record(jobConf,	{
			version=?THIS_JOB_VERSION,
			jobid,
			analysis_key,	%% added 28/8/2008
			moduleid,
			functionid,
			jobdesc,
			email,
			importSpec=#import{},
			logfile,
			logdir,
			filters=[],	%% output filters -- module specific
			filter		%% current filter
			}).

			
			

