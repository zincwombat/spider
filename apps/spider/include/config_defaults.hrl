-define(SRV_IMPORT_DIR, "/tmp").
-define(SRV_LOAD_DIR,   "/tmp").
-define(SRV_EXPORT_DIR, "/tmp").
-define(MAX_THREADS,    80).
-define(MAX_AGENTS,    400).

-include("embedded.hrl").

-define(DEFAULT_AGENTS,		10).
-define(AGENT_TIMEOUT,		30000).
-define(AGENT_SLEEP,		0).
-define(AGENT_MAXRQ,		unlimited).
-define(AGENT_DEFLOGGER,      	{rlogger_evh,notify}).
-define(AGENT_DEFGETTER,	{datasource,dget_a}).
-define(SPIDER_BASEDIR,		?BASEDIR ++ "/spider").
-define(SPIDER_UPLOAD_DIR,	?SPIDER_BASEDIR ++ "/upload").
-define(AGENT_DEFLOGDIR,     	?SPIDER_BASEDIR ++ "/results").
-define(AGENT_DEFLOGFILE,	"results.log").
-define(USER_CONFIG_FILE,	?SPIDER_BASEDIR++"/config/dg.xml").

-define(DATASOURCE_MAX_SIZE,	300000).


-define(RES_NS,         {127,0,0,1}).           %% localhost DNS
%%-define(RES_NS,         {192,168,50,206}).           %% wallaby.mit
-define(RES_ALT_NS,     {203,27,226,130}).      %% ns1.research

-define(RES_RSP_NS,     {203,27,227,123}).      %% rns1.melbourneit.com.au
-define(RES_ALT_RSP_NS, {203,27,227,124}).      %% rns2

-define(CONFIG_DEFAULTS,[

		%% DNS CONFIGURATION ---------------------------
		{'DNS_RES_NS',?RES_NS},
                {'DNS_RES_ALT_NS',?RES_ALT_NS},

		%% AGENT CONFIGURATION -------------------------
                {'DG_AGENT_MAXNUM',?MAX_AGENTS},
                {'DG_AGENT_DEFAULTNUM',?DEFAULT_AGENTS},
                {'DG_AGENT_TIMEOUT',?AGENT_TIMEOUT},
                {'DG_AGENT_SLEEP',?AGENT_SLEEP},
                {'DG_AGENT_MAXREQUESTS',?AGENT_MAXRQ},
		{'DG_AGENT_DEFAULT_LOGGER',?AGENT_DEFLOGGER},
		{'DG_AGENT_DEFAULT_GETTER',?AGENT_DEFGETTER},
		{'DG_AGENT_DEFAULT_LOGDIR',?AGENT_DEFLOGDIR},
		{'DG_AGENT_DEFAULT_LOGFILE',?AGENT_DEFLOGFILE},

		%% USER CONFIGURATION ---------------------------
		{'DG_USER_CONFIG_FILE',?USER_CONFIG_FILE},

		%% DATASOURCE CONFIGURATION ---------------------

		{'DG_DATASOURCE_MAX_SIZE',?DATASOURCE_MAX_SIZE},
		{'DG_DATASOURCE_DEFAULT_UPLOAD_DIR',?SPIDER_UPLOAD_DIR},

		{{monitor,srv_import_dir},?SRV_IMPORT_DIR},
                {{monitor,srv_load_dir},?SRV_LOAD_DIR},
                {{monitor,srv_export_dir},?SRV_EXPORT_DIR},
                {{monitor,export_file_suffix},"csv"},
                {{monitor,max_threads},?MAX_THREADS},
                {{monitor,import_file_suffix},"dmx"},
                {{monitor,load_file_suffix},"ets"},
                {{monitor,res_rsp_ns}, ?RES_RSP_NS},
                {{monitor,res_alt_rsp_ns}, ?RES_ALT_RSP_NS},
                {{monitor,res_ns},?RES_NS},
                {{monitor,res_alt_ns},?RES_ALT_NS}]).
