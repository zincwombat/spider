-module(job).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").
-export([serialise/1]).

-include("agent.hrl").

-define(NSURI,        "urn:analysis:job_v1").
-define(XSI,          "http://www.w3.org/2001/XMLSchema-instance").
-define(SCHEMALOC,    ?NSURI++" jobSchema.xsd").

%% note here that #job.moduleid is the identifier for the type of
%% xml content that the document contains 

serialise(Q=#controllerConf{version=Version,
		     res_ns=RES_NS,
		     res_alt_ns=RES_ALT_NS})->
	xmlmisc:mk_element(controller,[],"Controller");

serialise(Q=#jobConf{version=Version,
                     jobid=JobID,
                     jobdesc=JobDesc,
		     moduleid=ModuleID,
                     importSpec=#import{type=T,format=F,value=N},
                     logfile=LogFile})->

	{NSPrefix,ResultSetURI}=apply(list_to_atom(ModuleID),getNS,[]),

	ModuleDesc=
	case (catch apply(list_to_atom(ModuleID),getModuleDesc,[])) of
	{ok,D}->
		D;
	_->
		[]
	end,

	XMLNS=
	case NSPrefix of
	[]->
		"xmlns";
	P->
		"xmlns:"++P
	end,

        xmlmisc:startXml(job,[
				{"xmlns",?NSURI},
				{"xmlns:xsi",?XSI},
				{"xsi:schemaLocation",?SCHEMALOC},
                                {jobid,JobID},
				{moduleid,ModuleID},
                                {importtype,T},
                                {importfile,N},
                                {importformat,F},
                                {logfile,LogFile}])++
        xmlmisc:mk_element(moduleDesc,[],ModuleDesc)++
        xmlmisc:mk_element(jobdescription,[],JobDesc)++
	xmlmisc:startXml(resultSet,[{XMLNS,ResultSetURI}]);


serialise(endJob)->
        xmlmisc:endXml(resultSet)++
        xmlmisc:endXml(job);

serialise(PayLoad)->
        ?warn({unhandledSerialise,PayLoad}),
        [].

