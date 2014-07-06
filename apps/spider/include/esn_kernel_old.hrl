-define(DEFAULT_REPORT_DESTS,   [{erlang_error,both},
				 {erlang_info,both},
				 {esn_raise_alarm,both},
				 {esn_cease_alarm,both},
				 {esn_error,both},
				 {esn_progress,both},
				 {esn_trace,both},
				 {esn_statistics,both},
				 {sasl_supervisor,both},
				 {sasl_progress,both},
				 {sasl_crash,both}]).

-define(DEFAULT_EVENT_DESTS,   [ {erlang_error,{log,{true,log}}},
				 {erlang_info,{log,{true,log}}},
				 {esn_raise_alarm,{log,{true,log}}},
				 {esn_cease_alarm,{log,{true,log}}},
				 {esn_error,{log,{true,log}}},
				 {esn_progress,{log,{true,log}}},
				 {esn_trace,{log,{true,log}}},
				 {esn_statistics,{log,{true,log}}},
				 {sasl_supervisor,{log,{true,log}}},
				 {sasl_progress,{log,{true,log}}},
				 {sasl_crash,{log,{true,log}}}]).

%% definitions for esn_logger

-define(LOGDIR,		"/tmp").	%% dir for log files
-define(LOGFILE_MAXB,	100000).	%% max logfile size
-define(LOGFILE_MAXF,	2).		%% max no logfiles
-define(MAX_LOGFILES,	99).
-define(BACKUP_FILE_PREFIX,	"sav").
-define(INDEX_SUFFIX,		"index").

%% definitions for esn_event_mgr / esn_sysevent_mgr

-define(DEFAULT_REPORT_DEST,	local).
-define(DEFAULT_EVENT_DEST,	{log,{true,log}}).
-define(MAXQ_SIZE,		100).	%% queue size
-define(SYS_MAXQ_SIZE,		500).

-define(EVENT_DESTS,	[{erlang_error,{log,{true,log}}},
                         {erlang_info,{log,{true,log}}},
                         {esn_raise_alarm,{nolog,{true,log}}},
                         {esn_cease_alarm,{nolog,{true,log}}},
                         {esn_error,{nolog,{true,log}}},
                         {esn_progress,{log,{true,log}}},
                         {esn_trace,{log,{true,log}}},
                         {esn_statistics,{nolog,{true,nolog}}},
                         {sasl_supervisor,{log,false}},
                         {sasl_progress,{log,false}},
                         {sasl_crash,{log,false}}
			]).

%% definitions for esn_alarm_mgr

%% alarm_mode 1 means that alarms must be manually deleted by the operator
%% alarm_mode 2 means that alarms are automatically deleted when ceased

-define(DEFAULT_ALARM_MODE,	1).		%% default alarm mode
-define(DEFAULT_ALARM_MAX_AGE,	24).		%% Hours
-define(DEFAULT_ALARM_INITLOAD,	true).		%% load any saved alarms on process start
-define(DEFAULT_ALARMDIR,	"/tmp").	%% location to save alarms
-define(AGE_CHECK_INTERVAL,	3600000).	%% every 60 minutes, check for aged alarms
-define(ACK_CHECK_INTERVAL,	15000).		%% every 15 seconds, reactivate alarms unless
-define(ALARMSTATES,		[{active,new},	%% all possible alarm states
				 {active,timeout},
				 {active,repeat},
				 {ack,timeout},
				 {ack,notimeout},
			  	 {deleted,operator},
				 {deleted,auto},
				 {deleted,overflow},
				 {deleted,aged},
				 {deleted,forced},
				 ceased]).

-define(ALARM_DIR,		"/tmp").
-define(ALARM_LIST,		"esn_alarm_register.reg").
-define(ERROR_LIST,		"esn_error_register.reg").
-define(ALARM_SAVEFILE,		"alarmsave.ets").	%% name of file to save alarms on exit
-define(DEFAULT_MAXALARMS,	100).	%% default size of alarm table
-define(MINIMUM_MAXALARMS,	100).	%% table cannot be less than this size

%% definitions for esn_statistics


-define(STATS_SAVEFILE,		"statssave.ets").
-define(STATS_DIR,		"/tmp").

-define(STATS_DUMP_TIMEOUT,	360000).
-define(STATS_INITLOAD,		true).
-define(STATS_DUMP_DIR,		"/tmp").	
-define(STATS_DUMP_INTERVAL,	5).		
-define(STATS_DUMP_PREFIX,	"stats").	
