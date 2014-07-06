-module(objectMapper).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-include("agent.hrl").
-include("datasource.hrl").
-include("controller.hrl").
-include("cmanager.hrl").
-include("nstatus.hrl").
-include("urn_agent.hrl").
-include("urn_domain.hrl").
-include("spider.hrl").

-sysloglevel(?TRACE_INFO).
-export([map/1]).

map([S]) when is_tuple(S)->
	map(S);

map(undefined)->
	[];


map(NS=#nstatus{cm_state=#cm_state{},
		cc_state=#cc_state{},
		ds_state=#ds_state{}})->
	#dg_info{
		cm_state=map(NS#nstatus.cm_state),
		cc_state=map(NS#nstatus.cc_state),
		ds_state=map(NS#nstatus.ds_state)};

map(#cm_state{	cstate=CState,
		datasource=DataSource,
		jobConf=#jobConf{jobid=JobID,
				 moduleid=ModuleID,
				 jobdesc=JobDesc,
				 logfile=LogFile,
				 logdir=LogDir}})->
	#cm_info{
		cstate=?XSSTRING(cstate,[],CState),
		datasource=?XSSTRING(datasource,[],DataSource),
		jobid=?XSINT(jobid,JobID),
		jobdesc=?XSSTRING(jobdesc,[],JobDesc),
		logfile=?XSSTRING(logfile,[],LogFile),
		moduleid=?XSSTRING(module,[],ModuleID),
		logdir=?XSSTRING(logdir,[],LogDir)};

map(#cc_state{  aconf=AConf,
                cconf=CConf,
                curagents=CurAgents,
                start_tm=StartTime,
                stop_tm=StopTime})->
        #cc_info{       agentConf=#cc_agentConf{agentConf=AConf},
                        controllerConf=#cc_controllerConf{controllerConf=CConf},
                        curAgents=?XSINT(curAgents,CurAgents),
                        start_tm=?XSERLTIME(start_tm,StartTime),
                        stop_tm=?XSERLTIME(stop_tm,StopTime)};

map(#ds_state{  rq_count=RQ,
                data_count=DC,
                importSpec=IS,
                next=N,
                start_tm=Start,
                stop_tm=Stop,
                max_size=Max})->
        #ds_info{       rq_count=?XSINT(rq_count,RQ),
                        data_count=?XSINT(data_count,DC),
                        import_spec=#import_spec{import_spec=IS},
                        next=?XSSTRING(next,[],N),
                        start_tm=?XSERLTIME(start_tm,Start),
                        stop_tm=?XSERLTIME(stop_tm,Stop),
                        max_size=?XSINT(max_size,Max)
                };

map(#agentConf{
                invoke=Invoke,
                get=Get,
                logger=Logger,
                timeout=Timeout,
                sleep=Sleep,
                max_rq=MaxRq})->
        #ag_info{
                invoke=?XSERLFUN(invoke,Invoke),
                get=?XSERLFUN(get,Get),
                logger=?XSERLFUN(logger,Logger),
                timeout=?XSINT(timeout,Timeout),
                sleep=?XSINT(sleep,Sleep),
                max_rq=?XSINT(max_rq,MaxRq)
                };

map(SP=#agentResult{	
		version=Version,
		agentID=AgentId,
		timestamp=TS,
		payload=PayLoad})->
	?dbug({objectMapRequest,agentResult}),
	?dbug({payload,PayLoad}),
	#ag_payload{
		agentId=?XSSTRING(agentId,[],AgentId),
		timestamp=?XSERLTIME(timestamp,TS),
		payload=?XSCOMPLEX(payload,[],PayLoad)
		};

map(#controllerConf{
		numagents=NA,
		maxagents=MaxA,
		res_ns=ResNS,
		res_alt_ns=ResAltNS})->
	#cc_conf{numAgents=?XSINT(numagents,NA),
		 maxAgents=?XSINT(maxagents,MaxA),
		 res_ns=?XSIP4(res_ns,ResNS),
		 res_alt_ns=?XSIP4(res_alt_ns,ResAltNS)};

map(#dninfo{
		domain=Domain,
		status=Status,
		timestamp=Timestamp,
		soa=SOA,
		mxrrs=MXRRS,
		arrs=ARRS,
		nsrrs=NSRRS,
		facets=Facets})->
	#domain_info{
		domain=?XSSTRING(domain,[],Domain),
		timestamp=?XSERLTIME(timestamp,Timestamp),
		status=map(Status),
		soa=map(SOA),
		mxrrs=map(MXRRS),
		arrs=ARRS,
		nsrrs=NSRRS,
		facets=Facets};


map(#soa{	master=Master,
		email=Email,
		serial=Serial,
		refresh=Refresh,
		retry=Retry,
		expires=Expires,
		ttl=TTL})->
	#soa_info{master=?XSSTRING(master,[],Master),
		  email=?XSSTRING(email,[],Email),
		  serial=?XSINT(serial,Serial),
		  refresh=?XSINT(refresh,Refresh),
		  retry=?XSINT(retry,Retry),
		  expires=?XSINT(expires,Expires),
		  ttl=?XSINT(ttl,TTL)};

map(#mxrrs{mxrrs=[]})->
	?XSNULL(mxrrs);
map(#arrs{arrs=[]})->
	?XSNULL(arrs);
map(#nsrrs{nsrrs=[]})->
	?XSNULL(nsrrs);
map(#facets{facets=[]})->
	?XSNULL(facets);

map(#mxrrs{mxrrs=MXRRS}) when is_list(MXRRS)->
	V=lists:map(fun(Z)->map(Z) end,MXRRS),
	?XSSEQ(mxrrs,[],V);

map(#nsrrs{nsrrs=NSRRS}) when is_list(NSRRS)->
	V=lists:map(fun(Z)->map(Z) end,NSRRS),
	?XSSEQ(nsrrs,[],V);

map(#arrs{arrs=ARRS}) when is_list(ARRS)->
	V=lists:map(fun(Z)->map(Z) end,ARRS),
	?XSSEQ(arrs,[],V);

map(#facets{facets=F}) when is_list(F)->
	V=lists:map(fun(Z)->map(Z) end,F),
	?XSSEQ(facets,[],V);

map(#facet{facet=F,topics=T}) when is_list(T)->
	V=lists:map(fun(Z)->map(Z) end,T),
	?XSSEQ(facet,[{name,F}],V);

map(#topic{topic=T})->
	?XSSTRING(topic,[],T);

map(#mxrr{server=Server,prio=Prio})->
	?XSSTRING(server,[{prio,Prio}],Server);

map(#nsrr{server=Server})->
	?XSSTRING(server,[],Server);

map(#arr{type=ip4,addr=Addr})->
	?XSIP4(address,Addr);

map(#status{status={error,{Class,Reason}}})->
	?XSSTRING(error,[{class,Class}],Reason);

map(#status{status={error,Reason}})->
	?XSSTRING(error,[],Reason);

map(#status{status=Other})->
	?XSSTRING(status,[],Other);

map(Other)->
	?warn({noMapping,Other}),
	[].





