-module(esn_report).


-export([	error/3,
		error/4,
		format/2,
		raise_alarm/4,
		raise_alarm/3,
		cease_alarm/3,
		cease_alarm/2,
         	progress/2,
		trace/2,
		debug/2,	%% added 5/1/2005
		info/2,		%% ditto
		warn/2,		%% ditto
		critical/2,	%% ditto
		usage/3,
		usage/2]).

esn_format(Type,Args)->
	{{date(),time()},node(),{error_report,gl,{self(),esn_report,{Type,Args}}}}.

%%% EXTERNAL API -------------------------------------------------------------------------

format(error,{Location,ErrorId,ErrorInfo})->
	esn_format(esn_error,{Location,ErrorId,ErrorInfo,use_default_severity});

format(error,Args)->
	esn_format(esn_error,Args);

format(raise_alarm,{Location,AlarmId,AlarmInfo})->
	esn_format(esn_raise_alarm,{Location,AlarmId,AlarmInfo,no_instance});

format(raise_alarm,Args)->
	esn_format(esn_raise_alarm,Args);

format(cease_alarm,{Location,AlarmId})->
	esn_format(esn_cease_alarm,{Location,AlarmId,no_instance});

format(cease_alarm,Args)->
	esn_format(esn_cease_alarm,Args);

format(progress,Args)->
	esn_format(esn_progress,Args);

format(trace,Args)->
	esn_format(esn_trace,Args);

format(usage,{Location,UsageCounter})->
	esn_format(esn_statistics,{Location,UsageCounter,1});

format(usage,Args)->
	esn_format(esn_statistics,Args);

format(Other,Args)->
	{error,{badargs,{Other,Args}}}.

error(Location,ErrorId,ErrorInfo) ->
	error_logger:error_report(esn_report, {	esn_error,
						{ 	Location,
							ErrorId,
							ErrorInfo,
							use_default_severity
						}
					      }).

error(Location,ErrorId,ErrorInfo,sysfatal)->
	error_logger:error_report(esn_report, { esn_error,
						{	Location,
							ErrorId,
							ErrorInfo,
							sysfatal
						}
					      }),
	esn_kernel:sysfatal(Location,ErrorId,ErrorInfo);
	
error(Location,ErrorId,ErrorInfo,Severity)->
	error_logger:error_report(esn_report, { esn_error,
						{	Location,
							ErrorId,
							ErrorInfo,
							Severity
						}
					      }).

raise_alarm(Location,AlarmId,AlarmInfo,AlarmInstance) ->
	error_logger:error_report(esn_report, {	esn_raise_alarm,
						{	Location,
							AlarmId,
							AlarmInfo,
							AlarmInstance
						}
						}).

raise_alarm(Location,AlarmId,AlarmInfo) ->
	error_logger:error_report(esn_report, {	esn_raise_alarm,
						{	Location,
							AlarmId,
							AlarmInfo,
							no_instance
						}
						}).

cease_alarm(Location,AlarmId,AlarmInstance) ->
	error_logger:error_report(esn_report, {	esn_cease_alarm,
						{	Location,
							AlarmId,
							AlarmInstance
						}
						}).

cease_alarm(Location,AlarmId) ->
	error_logger:error_report(esn_report, {	esn_cease_alarm,
						{	Location,
							AlarmId,
							no_instance
						}
						}).

progress(Location,ProgressInfo) ->
	error_logger:info_report(esn_report, {	esn_progress,
						{	Location,
							ProgressInfo
						}
						}).

trace(Location,TraceInfo) ->
	error_logger:info_report(esn_report, {	esn_trace,
						{	Location,
							TraceInfo
						}
						}).

%% following 4 functions added 5/1/2005

debug(Location,TraceInfo) ->
	error_logger:info_report(esn_report, {	esn_debug,
						{	Location,
							TraceInfo
						}
						}).

info(Location,TraceInfo) ->
	error_logger:info_report(esn_report, {	esn_info,
						{	Location,
							TraceInfo
						}
						}).

warn(Location,TraceInfo) ->
	error_logger:info_report(esn_report, {	esn_warn,
						{	Location,
							TraceInfo
						}
						}).

critical(Location,TraceInfo) ->
	error_logger:info_report(esn_report, {	esn_critical,
						{	Location,
							TraceInfo
						}
						}).

usage(Location,UsageCounter,Increment) ->
	error_logger:info_report(esn_report, {	esn_statistics,
						{	Location,
							UsageCounter,
							Increment
						}
						}).

usage(Location,UsageCounter) ->
	error_logger:info_report(esn_report, {	esn_statistics,
						{	Location,
							UsageCounter,
							1
						}
						}).

