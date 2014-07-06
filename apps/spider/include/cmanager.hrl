-include("xmlinfo.hrl").

-undef(THIS_XMLNS).
-define(THIS_XMLNS,             xmlinfo=?XML(?URN_CM)).

-record(cm_state,{
		cstate,
               	datasource,
		configfile,
		agentConf,
		controllerConf,
		jobConf,
		def_agentConf,
		def_controllerConf,
		def_jobConf
}).

-define(CM_IMPORT_TMOUT,        240000).
-define(CM_ABORT_TMOUT,         10000).

-record(cm_info,{?THIS_XMLNS,cstate,datasource,jobid,jobdesc,moduleid,logfile,logdir}).
