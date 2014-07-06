-module(urn_domain).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-include("http.hrl").
-include("agent.hrl").
-include("urn_domain.hrl").
-include("domaininfo.hrl").
-include("filter.hrl").


-define(DNSFAIL_FILTER,	#filter_opt{key=dnsFail,type=?FILETYPE_CSV,desc="DNS Failure"}).
-define(NXDOMAIN_FILTER,#filter_opt{key=nxdomain,type=?FILETYPE_CSV,desc="DNS NXDOMAIN"}).
-define(A_FILTER,	#filter_opt{key=a,type=?FILETYPE_CSV,desc="A Records"}).
-define(AUTHNS_FILTER,	#filter_opt{key=authns,type=?FILETYPE_CSV,desc="Authoritative NS"}).
-define(ALL_FILTER,	#filter_opt{key=all,type=?FILETYPE_CSV,desc="All Data Available"}).
-define(ALL_FILTER_XML,	#filter_opt{key=xmlall,type=?FILETYPE_XML,desc="All Data Available"}).

-define(FILTER_LIST,	[?DNSFAIL_FILTER,?NXDOMAIN_FILTER,?A_FILTER,?AUTHNS_FILTER,?ALL_FILTER,?ALL_FILTER_XML]).

-define(RESULT_ROOT,	result).

-export([allCheck/1]).
-export([webCheck/1]).
-export([mailCheck/1]).
-export([contentCheck/1]).
-export([dnsCheck/1]).
-export([delegationCheck/1]).
-export([loopBack/1]).

-export([dnsCheckXml/2]).
-export([allCheckXml/2]).

-export([preCheck/1]).
-export([check_all/1]).

%% Note: filter is now specified by the tuple {Module,Type}
%% it is assumed that all modules implement the callback
%% function Module:filter/2

-export([filter/2]).
-export([getFilters/0]).

-export([filterAuthNs/1]).
-export([filterA/1]).
-export([filterDnsFail/1]).
-export([filterNxDomain/1]).
-export([extractNS/1]).
-export([extractNS/2]).
-export([check/2]).

-export([test/1]).

-define(THIS_XMLNS,	xmlinfo=?XML(?URN_DOMAIN)).

-export([serialise/1,
	 i_serialise/1,
	 getNS/0,
	 getSchema/0,
	 getXSL/0,
	 getXSL/1]).

-export([test/1]).
-sysloglevel(?TRACE_WARN).


-define(NSURI,		"urn:analysis:check9").
-define(SCHEMALOC,	"check9.xsd").
-define(XSL,		"check9.xsl").


addCStatus(KV={Key,Value},DNInfo=#dninfo{cstatus={cstatus,CSList}}) when is_list(CSList)->
	NewCSList=lists:append([KV],CSList),
	DNInfo#dninfo{cstatus={cstatus,NewCSList}}.
	

%%=============================================================================
%% BEGIN Canonical checks
%%=============================================================================

preCheck(HName)->
	case inet_parse:domain(HName) of
	true->
		DNInfo=#dninfo{domain=HName,
			       tm_start=erlang:now()},

		case dnutils:mkDnInfo(HName) of
		E={error,{unknown_tld,TLD}}->
			?warn({unknown_tld,{tld,TLD},{fqdn,HName}}),
			DNInfo#dninfo{basedn_status=E,tld=TLD};

		E={error,Reason}->
			DNInfo#dninfo{basedn_status=E};
		
		DI when is_record(DI,domain)->
			case ns2:authNs(HName) of
			{ns,[SOA={_,_,_,_,_,_,_}]}->
				?warn({soa_returned_on_authns,{domain,HName},{soa,SOA}}),
				DNInfo#dninfo{	tld=DI#domain.space,
						basedn_status=wildcard,
					      	basedn=DI#domain.rname,
					      	cstatus={cstatus,[{nsa,wildcard}]}};
				
			{ns,E={error,nxdomain}}->
				DNInfo#dninfo{	tld=DI#domain.space,
						basedn_status=nxdomain,
					      	basedn=DI#domain.rname,
					      	cstatus={cstatus,[{nsa,nxdomain}]}};

			{ns,E={error,Reason}}->
				DNInfo#dninfo{	tld=DI#domain.space,
						basedn_status=E,
					      	basedn=DI#domain.rname,
					      	cstatus={cstatus,[{nsa,Reason}]}};

			NSA={ns,NS=[H|T]}->
				N=(catch i_mkrec(ns,NS)),
				IsCname=
				case ns2:getRR(cname,HName) of
				{cname,{error,Reason}}->
					false;
				Other->
					true
				end,
				DNInfo#dninfo{	isCname=IsCname,
						tld=DI#domain.space,
						basedn_status=delegated,
						basedn=DI#domain.rname,
					      	basedn_delegations={basedn_delegations,{nsrrs,N}},
					      	cstatus={cstatus,[{nsa,delegated}]}}	
			end
		end;

	_->
		%% TODO -- make sure in a catch loop
		throw({{module,?MODULE},{error,{badarg,HName}}})
	end.

test(HName)->
	{_,D}=check(HName,isp),
	XML=serialise(all,D),
	lists:flatten(XML).

check_all(HName)->
	check(HName,all).

check(HName,isp)->
	check(HName,[dns,isp]);

check(HName,all)->
	check(HName,[dns,mail,web,isp]);

check(HName,Type) when is_atom(Type)->
	check(HName,[Type]);

check(HName,Type) when is_list(Type)->
	HNameCanonical=string:to_lower(HName),
	CheckResult=
	case preCheck(HNameCanonical) of 
	DNInfo=#dninfo{basedn_status=nxdomain}->
		DNInfo;

	DNInfo=#dninfo{basedn_status={error,Reason}}->
		DNInfo;

	DNInfo=#dninfo{}->
		Result=lists:foldl(fun(Z,Acc)->x_check(HNameCanonical,Z,Acc) end,DNInfo,Type),
		Result;

	Other->
		%% TODO -- check this
		throw(Other)
	end,
	{{module,?MODULE},CheckResult#dninfo{tm_stop=erlang:now()}}.

x_check(HName,Type,DNInfo)->
	%% record the type of analysis performed in the #dninfo{} record
	i_check(HName,Type,addCheck(Type,DNInfo)).

i_check(HName,dns,DNInfo=#dninfo{})->
	%% first check if there is a CNAME and if so, just get the A records
	RRList=
	case DNInfo#dninfo.isCname of
	true->
		[cname,a];
	_->
		[ns,a,mx,soa]
	end,
	DNSInfo=ns2:getRR(RRList,HName), 
	Result=(catch mkrec(DNSInfo,DNInfo)),
        DNSFacet=facet:invoke(HName,dns),
	addFacet(DNSFacet,Result);

i_check(HName,isp,DNInfo=#dninfo{arrs=ARRs})->
	ISPs=getISP(ARRs),
	Result=DNInfo#dninfo{isps=ISPs},
	?info({result,Result}),
	Result;

i_check(HName,delegation,DNInfo)->
	i_check(HName,dns,DNInfo);

i_check(HName,web,DNInfo)->
	WebFacet=facet:invoke(HName,web),
	?info({webfacet,WebFacet}),
	addFacet(WebFacet,DNInfo);

i_check(HName,mail,DNInfo)->
	MailFacet=facet:invoke(HName,mail),
	?info({mailfacet,MailFacet}),
	addFacet(MailFacet,DNInfo);

i_check(HName,all,DNInfo)->
	EDNInfo=DNInfo,
	EDNInfo;

i_check(HName,Other,DNInfo)->
	?warn({unknown_check,Other}),
	DNInfo.

addCheck(Check,DNInfo=#dninfo{checks=Cs}) when is_list(Cs)->
	DNInfo#dninfo{checks=lists:append(Cs,[Check])}.

addFacet({_HName,Facet={F,Ts}},DNInfo)->
	addFacet(Facet,DNInfo);

addFacet(Facet={F,Ts},DNInfo=#dninfo{facets=#facets{facets=Fs}}) when is_list(Fs)->
	%% TODO check if the facet already exists and if so, replace it
	NewFacets=lists:append(Fs,[Facet]),
	DNInfo#dninfo{facets=#facets{facets=NewFacets}};

addFacet(Facet={F,Ts},DNInfo=#dninfo{facets=Facets})->
	DNInfo#dninfo{facets=#facets{facets=[Facet]}}.
	

%%=============================================================================
%% END Canonical checks
%%=============================================================================

getISP({arrs,Arrs}) when is_list(Arrs)->
	ISPs=lists:map(fun(Z)->{Z,geoip:lookup(Z)} end,Arrs),
	{isps,procISP(ISPs,[])};

getISP(RRs) when is_list(RRs)->
	case lists:keyfind(a,1,RRs) of
	A={a,Arrs}->
		ISPs=lists:map(fun(Z)->{Z,geoip:lookup(Z)} end,Arrs),
		{isps,procISP(ISPs,[])};
		
	false->
		{isps,[]}
	end;

getISP(Other)->
	{isps,[]}.

procISP([],Acc)->
	Acc;

procISP([Z={{arr,{{addr,IP={A,B,C,D}},_}},{ok,ISP}}|T],Acc)->
	procISP(T,lists:append(Acc,[{isp,IP,ISP}]));

procISP([Z={IP={A,B,C,D},{ok,ISP}}|T],Acc)->
	procISP(T,lists:append(Acc,[{isp,IP,ISP}]));

procISP([Z={#arr{addr=IP,type=ip4},{ok,ISP}}|T],Acc)->
	procISP(T,lists:append(Acc,[{isp,IP,ISP}]));

procISP([Z={IP={A,B,C,D},Other}|T],Acc)->
	procISP(T,Acc);

procISP([Z|T],Acc)->
	procISP(T,Acc).

contentCheck(Arg)->
	Content=ihttpc:hget(Arg),
	?dbug({content,Content}),
	Result=facetmap_webcontent_v1:spider(Arg,Content),
	{{module,?MODULE},Result}.

allCheckXml(Arg,Tag)->
	{_,AllCheck}=allCheck(Arg),
	lists:flatten(serialise(Tag,AllCheck)).

allCheck(Arg)->
	i_allCheck(Arg,ns2:getRR(nsa,Arg)).

i_allCheck(Arg,ZZ={_,{error,nxdomain}})->
	%% the baseDN is undelegated ...
	DNSInfo=[{ns,{error,nxdomain}}],
	R1=f(dnutils:dnsCheck(Arg)),
	RH=facetmap_web_v1:classify(nxdomain),
	R2=f({Arg,RH}),  
	R3=f(imail:mcheck(Arg)), 
	Z=#dninfo{timestamp=erlang:now(),
                   domain=Arg,
		   status=#status{status=nxdomain},
                   facets=#facets{facets=[R1,R2,R3]}},
        Result=(catch mkrec(DNSInfo,Z)),
	{{module,?MODULE},Result};

i_allCheck(Arg,_)->
	DNSInfo=ns2:getRR([nsa,a,mx,soa],Arg),
        ?dbug({ns2,DNSInfo}),
        R1=f(dnutils:dnsCheck(Arg)),
        RH=facetmap_web_v1:classify(ihttpc:xhead(Arg)),
        %%RH=facetmap_web_v1:classify(ihttpc:head(Arg)),
        R2=f({Arg,RH}),
        R3=f(imail:mcheck(Arg)),
        Result=(catch mkrec(DNSInfo,#dninfo{timestamp=erlang:now(),
                                             domain=Arg,
                                             facets=#facets{facets=[R1,R2,R3]}
                                })),
        ?info({result,Result}),
        {{module,?MODULE},Result}.

delegationCheck(Arg)->
	DNSInfo=ns2:getRR([nsa],Arg), 
        R2=f(dnutils:dnsCheck(Arg)),
	Result=(catch mkrec(DNSInfo,#dninfo{timestamp=erlang:now(),
					     domain=Arg,
					     facets=#facets{facets=[R2]}
				})),
	?info({result,Result}),
	{{module,?MODULE},Result}.

	

webCheck(Arg)->
	RH=facetmap_web_v1:classify(ihttpc:xhead(Arg)),
	R1=f({Arg,dnutils:nullFacet()}),
        R2=f({Arg,RH}),
	R3=f({Arg,imail:nullFacet(Arg)}),
	Result=(catch mkrec([],#dninfo{timestamp=erlang:now(),
					     domain=Arg,
					     facets=#facets{facets=[R1,R2,R3]}
				})),
	?info({result,Result}),
	{{module,?MODULE},Result}.

mailCheck(Arg)->
        R2=f(imail:mcheck(Arg)),
	Result=(catch mkrec([],#dninfo{timestamp=erlang:now(),
					     domain=Arg,
					     facets=#facets{facets=[R2]}
				})),
	?info({result,Result}),
	{{module,?MODULE},Result}.


dnsCheckXml(Arg,Tag)->
	{_,DNSCheck}=dnsCheck(Arg),
	lists:flatten(serialise(Tag,DNSCheck)).
	

dnsCheck(Arg)->
	DNSInfo=ns2:getRR([nsa,a,mx,soa],Arg), 
        R2=f(dnutils:dnsCheck(Arg)),
	Result=(catch mkrec(DNSInfo,#dninfo{timestamp=erlang:now(),
					     domain=Arg,
					     facets=#facets{facets=[R2]}
				})),
	?info({result,Result}),
	{{module,?MODULE},Result}.

loopBack(Arg)->
	{{module,?MODULE},null}.


%%=============================================================================
%% Root Element Serialiser
%%=============================================================================

serialise(DNI=#domain_info{})->
        XML=serialise(complex,?URN_DOMAIN_PFX,DNI),
        lists:flatten(XML);

serialise(Other)->
	?warn({noserialiser,Other}),
	[].

%==============================================================================
% Complex content type serialisers - serialise/3
%==============================================================================

getAttributes(_)->
	[].


serialise(_Type,Pfx,{Tag,#xmlinfo{},undefined})->
        xmlmisc:mk_element(Tag,[],[],Pfx);

serialise(complex,Pfx,Complex) when is_tuple(Complex)->
        case tuple_to_list(Complex) of
        [Tag,#xmlinfo{xmlns=?URN_DOMAIN}|Content]->
                %% correct namespace
                ?dbug({serialisingComplexType,Complex}),
                Attributes=getAttributes(Complex),
                xmlmisc:startXml(Tag,Attributes,Pfx) ++
                lists:map(fun(Z)->serialise(Pfx,Z) end,Content) ++
                xmlmisc:endXml(Tag,Pfx);

        [Tag|Content]->
                %% assume local namespace anyway ....
                ?dbug({serialisingComplexTypeAssumeLocalNS,Complex}),
                Attributes=getAttributes(Complex),
                xmlmisc:startXml(Tag,Attributes,Pfx) ++
                lists:map(fun(Z)->serialise(Pfx,Z) end,Content) ++
                xmlmisc:endXml(Tag,Pfx);

        _Other->
                ?warn({complexNoSerialiser,Complex}),
                []
        end;

serialise(_Other,_Pfx,O={_Tag,#xmlinfo{},_Value})->
        ?warn({noserialiser,O}),
        [];

serialise(_Type,_Pfx,undefined)->
        [].


%%=============================================================================
%%=============================================================================
%% Content Serialisers - serialise/2
%%=============================================================================
%%=============================================================================

serialise(RootElement,DNInfo=#dninfo{agent_id=AgentID})->
	RecFields=record_info(fields,dninfo),
	RecSize=length(RecFields),
	Accessor=lists:zip(RecFields,lists:seq(2,RecSize+1)),
	Payload=lists:map(fun(Z)->z_serialise(Z,DNInfo,Accessor) end,RecFields),
	xmlmisc:mk_element(RootElement,Payload).

z_serialise(FieldName,DNInfo,Accessor)->
	{FieldName,Index}=lists:keyfind(FieldName,1,Accessor),
	Data=element(Index,DNInfo),
	zz_serialise(FieldName,Data).

zz_serialise(FieldName,Data)->
	case io_lib:printable_list(Data) of
	true->
		xmlmisc:mk_element(FieldName,Data);
	false->
		custom(FieldName,Data)
	end.

custom(Tag,X={cstatus,CStatus}) when is_list(CStatus)->
	Payload=lists:map(fun(Z)->xmlmisc:mk_element(element(1,Z),element(2,Z)) end,CStatus),
	xmlmisc:mk_element(cstatus,Payload);

custom(Tag,X={soa,SoaParams}) when is_tuple(SoaParams)->
	Payload=lists:map(fun(Z)->xmlmisc:mk_element(element(1,Z),element(2,Z)) end,tuple_to_list(SoaParams)),
	xmlmisc:mk_element(soa,Payload);

custom(Tag,X={soa,_,_,_,_,_,_,_})->
	xmlmisc:mk_element(soa,[]);

custom(Tag,X={{facet,Facet},TopicList}) when is_list(TopicList)->
	Payload=lists:map(fun(Z)->xmlmisc:mk_element(element(1,Z),element(2,Z)) end,TopicList),
	xmlmisc:mk_element(facet,[{id,Facet}],Payload);

custom(checks,CheckList) when is_list(CheckList)->
	Payload=lists:map(fun(Z)->xmlmisc:mk_element(check,Z) end,CheckList),
	xmlmisc:mk_element(checks,Payload);

custom(basedn_status,{error,{unknown_tld,TLD}})->
	xmlmisc:mk_element(basedn_status,[{error,true}],"unknown TLD: " ++ TLD);

custom(basedn_status,{error,{unhandled,Domain}})->
	xmlmisc:mk_element(basedn_status,[{error,true}],"domain not supported: " ++ Domain);

custom(basedn_status,{error,Reason}) when is_atom(Reason)->
	xmlmisc:mk_element(basedn_status,[{error,true}],"error: " ++ atom_to_list(Reason));

custom(Tag,X={basedn_delegations,NS={nsrrs,NSList}})->
	Payload=zz_serialise(nsrrs,NSList),
	xmlmisc:mk_element(basedn_delegations,Payload);

custom(Tag,X={arr,{{addr,{A,B,C,D}},{type,ip4}}})->
	Payload=lists:flatten(io_lib:format("~w.~w.~w.~w",[A,B,C,D])),
	zz_serialise(arr,Payload);

custom(Tag,X={isp,{A,B,C,D},ISP})->
	IP4=lists:flatten(io_lib:format("~w.~w.~w.~w",[A,B,C,D])),
	xmlmisc:mk_element(isp,[{addr,IP4}],ISP);

custom(Tag,X={mxrr,{{server,Server},{prio,Prio}}})->
	xmlmisc:mk_element(mxrr,[{prio,Prio}],Server);

custom(Tag,X={nsrr,Nameserver})->
	xmlmisc:mk_element(nsrr,Nameserver);

custom(Tag,X={cname,Nameserver})->
	xmlmisc:mk_element(cname,Nameserver);

custom(Tag,EN={_,_,_}) when Tag==tm_stop; Tag==tm_start->
	TimeNow=xmlmisc:formatUTC(EN),
	zz_serialise(Tag,TimeNow);

custom(Tag,Data)->
	default_serialiser(Tag,Data).

default_serialiser(Tag,Data) when is_atom(Data)->
	zz_serialise(Tag,atom_to_list(Data));

default_serialiser(Tag,X={Tag,Data})->
	zz_serialise(Tag,Data);

default_serialiser(Tag,X=[H|_T]) when is_tuple(H)->
	Payload=lists:map(fun(Z)->zz_serialise(Tag,Z) end, X),
	xmlmisc:mk_element(Tag,Payload);

default_serialiser(Tag,Data)->
	?critical({no_serialiser,{tag,Tag},{data,Data}}),
	[].

%%=============================================================================
%% END
%%=============================================================================


f({_Domain,{Facet,Cs}})->
        #facet{facet=Facet,topics=mkTopics(Cs)}.

mkTopics(Ts) when is_list(Ts),is_integer(hd(Ts))->
        #topic{topic=Ts};

mkTopics(Ts) when is_list(Ts)->
        lists:map(fun(Z)->#topic{topic=Z} end,Ts).

mkrec(RRs,DNS=#dninfo{}) when is_list(RRs)->
	case (catch lists:foldl(fun(Z,Acc)->mkrec(Z,Acc) end,DNS,RRs)) of
	E={error,_Reason}->
		?warn(E),
		throw(DNS#dninfo{status=#status{status=E}});

	{E={error,_Reason},EDNS}->
		?dbug(E),
		throw(EDNS#dninfo{status=#status{status=E}});

	Other->
		Other
	end;

mkrec(Z={RRType,E={error,Reason}},DNInfo=#dninfo{})->
	addCStatus({RRType,0},DNInfo);

mkrec({soa,[SOA={_,_,_,_,_,_,_}]},DNS)->
	addCStatus({soa,1},DNS#dninfo{soa=i_mkrec(soa,SOA)});

mkrec({soa,SOA},DNS=#dninfo{domain=Domain})->
	?info({bad_soa,{domain,Domain},SOA}),
	addCStatus({soa,0},DNS);

mkrec({a,ARRs},DNS)->
	DL=(catch i_mkrec(a,ARRs)),
	A=lists:flatten(DL),
	addCStatus({a,length(A)},DNS#dninfo{arrs=#arrs{arrs=A}});

mkrec({mx,MXRRs},DNS)->
	M=
	case (catch i_mkrec(mx,MXRRs)) of
	cname->
		%% TODO -- handle this case
		[];
	MX->
		MX
	end,
	addCStatus({mx,length(M)},DNS#dninfo{mxrrs=#mxrrs{mxrrs=M}});

mkrec({ns,NSRRs},DNS)->
	N=(catch i_mkrec(ns,NSRRs)),
	addCStatus({ns,length(NSRRs)},DNS#dninfo{nsrrs=#nsrrs{nsrrs=N}});

mkrec({cname,CNAME},DNS)->
	C=(catch i_mkrec(cname,CNAME)),
	addCStatus({cname,length(CNAME)},DNS#dninfo{cnamerrs=#cnamerrs{cnamerrs=C}});


mkrec(Other,DNS)->
	?dbug({mkrecUnHandled,Other}),
	DNS.

i_mkrec(RRType,E={error,_Reason})->
	?warn({i_mkrec,{rr,RRType},E}),
	throw(E);

i_mkrec(RRType,RRs) when is_list(RRs),not is_integer(hd(RRs))->
	lists:map(fun(Z)->i_mkrec(RRType,Z) end,RRs);

%% A Record handling ==========================================================

i_mkrec(a,IP={_A,_B,_C,_D})->
%% FIX	#arr{type=ip4,addr=IP};
	{arr,{{addr,IP},{type,ip4}}};

i_mkrec(a,[])->
	[];

i_mkrec(a,RRs)->
	%% must be a CNAME, so ignore
	?dbug({cnameOnArec,RRs}),
	[];


%% CNAME  Record handling =========================================================

i_mkrec(cname,[])->
	[];

i_mkrec(cname,CNAME)->
	#cname{cname=CNAME};

%% MX Record handling =========================================================

i_mkrec(mx,{Prio,Server})->
%% FIX 	#mxrr{prio=Prio,server=Server};
	{mxrr,{{server,Server},{prio,Prio}}};

i_mkrec(mx,[])->
	[];

i_mkrec(mx,RRs)->
	%% must be a CNAME, so ignore
	?dbug({cnameOnMXRec,RRs}),
	[];

%% NS Record handling =========================================================

i_mkrec(ns,RRs)->
	#nsrr{server=RRs};


%% SOA Record Handling=========================================================

i_mkrec(soa,{Master,Email,Serial,Refresh,Retry,Expires,TTL})->
%	#soa{	master=Master,
%		email=Email,
%		serial=Serial,
%		refresh=Refresh,
%		retry=Retry,
%		expires=Expires,
%		ttl=TTL};
	{soa,{	{master,Master},
		{email,Email},
		{serial,Serial},
		{refresh,Refresh},
		{retry,Retry},
		{expires,Expires},
		{ttl,TTL}}};

i_mkrec(soa,[])->
%	#soa{};
	{soa,	undefined};

i_mkrec(soa,Other)->
	?dbug({soa_other,Other}),
	case utils:is_string(Other) of
	true->
		?info({cnameOnSOARec,Other}),
%		#soa{};
		{soa,undefined};
	false->
		E={error,{badAddress,{soa,Other}}},
		?warn(E),
		throw(E)
	end;

%% CATCH ALL ==================================================================

i_mkrec(RRType,Other)->
	E={error,{badAddress,RRType,Other}},
	?warn(E),
	throw(E).

% SERIALISER ==================================================================

i_serialise(Object)->
        ?dbug({noSerialise,Object}),
	throw({error,{noserialiser,Object}}).

%===============================================================================

getNS()->
	{?URN_DOMAIN_PFX,?URN_DOMAIN}.

getSchema()->
	{?URN_DOMAIN,?SCHEMALOC}.

getXSL()->
	getXSL(default).

getXSL(_UA)->
	{ok,xmlmisc:xmlxsl(?XSL)}.

%==============================================================================	
% These are the filters that may be used by rlogger:addFilter/2 to replace the
% standard XML serialisers 
%==============================================================================

getFilters()->
	?FILTER_LIST.


%==============================================================================
% BEGIN XML filters added 21/6/2014
%==============================================================================

filter(xmlall,startJob)->
	{filter,xmlmisc:startXml(root,[{xmlns,?URN_DOMAIN}])};

filter(xmlall,endJob)->
	{filter,xmlmisc:endXml(results)++xmlmisc:endXml(root)};

filter(xmlall,CC=#controllerConf{
		version=Version,
                numagents=NumAgents,
                res_ns=Res_NS})->
        {filter,xmlmisc:mk_element(control,[{version,Version},{numagents,NumAgents}],[]) ++ xmlmisc:startXml(results,[])};

filter(xmlall,JC=#jobConf{
                version=Version,
                jobid=JobId,
                moduleid=ModuleId,
                jobdesc=JobDesc,
                importSpec=#import{type=Type,format=Format,value=Value},
                logfile=LogFile,
                logdir=LogDir})->
        {filter,xmlmisc:mk_element(job,[{version,Version},{moduleid,ModuleId},{sourcetype,Type},{format,Format},{value,Value}],[])};

filter(xmlall,#agentResult{
		agentID=AgentID,
		payload=P={{module,Module},DNInfo=#dninfo{}}})->
	XML=serialise(?RESULT_ROOT,DNInfo#dninfo{agent_id=AgentID,
						 agent_timeout=false}),
	{filter,XML};


filter(xmlall,#agentResult{
		agentID=AgentID,
		timestamp=TS,
		payload=P={systimeout,{data,Domain},{limit,Limit}}})->
	?info({xmlall,{payload,P}}),
	XML=serialise(?RESULT_ROOT,#dninfo{agent_id=AgentID,
					   domain=Domain,
					   agent_timeout=true}),
	{filter,XML};

%==============================================================================
% END XML filters added 21/6/2014
%==============================================================================


filter(Type,#agentResult{payload=PayLoad})->
	filter(Type,PayLoad);

filter(Type,{_,DNSInfo=#dninfo{domain=Domain,status=#status{status={error,{dns,nxdomain}}},facets=Facets}})->
	X=io_lib:format("~s,nxdomain",[Domain]),
	{filter,X};

filter(dnsFail,{_,DNSInfo=#dninfo{domain=Domain,facets=Facets}})->
	case extractFacets(Facets,"urn:facetmap:dns_v1","dnsFail") of
        true->
                X=io_lib:format("~s,",[Domain]),
                {filter,X};
        _->
                {filter,null}
        end;

filter(nxdomain,{_,DNSInfo=#dninfo{domain=Domain,facets=Facets}})->
	case extractFacets(Facets,"urn:facetmap:dns_v1","nxdomain") of
        true->
                X=io_lib:format("~s,",[Domain]),
                {filter,X};
        _->
                {filter,null}
        end;

filter(a,{_,DNSInfo=#dninfo{domain=Domain,status=Status,arrs=ARRS}})->
	AList=extractA(DNSInfo),
	X=io_lib:format("~s,delegated,",[Domain])++AList,
	{filter,X};

filter(authns,{_,DNSInfo=#dninfo{domain=Domain,status=Status,nsrrs=NSRRS}})->
	X=
	case Status of
	#status{status={error,{dns,nxdomain}}}->
		io_lib:format("~s,nxdomain",[Domain]);
	_->
		NSList=extractNS(DNSInfo),
		io_lib:format("~s,delegated,",[Domain])++NSList
	end,
	{filter,X};

filter(all,{_,DNSInfo=#dninfo{	domain=Domain,
				status=Status,
				nsrrs=NSRRS,
				facets=Facets}})->
	NSList=extractNS(DNSInfo),
	{_,F1}=extractFacetLeaf(Facets,"urn:facetmap:dns_v1"),
	%% {_,F2}=extractFacetLeaf(Facets,"urn:facetmap:web_v1"),
	{_,F3}=extractFacetLeaf(Facets,"urn:facetmap:mail_v1"),
	%% F1=extractFacetLeaf(Facets,"urn:facetmap:dns_v1"),
	F2=extractFacetLeaf(Facets,"urn:facetmap:web_v1"),
	%% F3=extractFacetLeaf(Facets,"urn:facetmap:mail_v1"),
	?dbug({f1,F1}),
	?dbug({f2,F2}),
	?dbug({f3,F3}),
	F=io_lib:format("~s,~s,~s,",[F1,F2,F3]),
	X=io_lib:format("~s,",[Domain])++F++NSList,
	{filter,X};

%% special filter for the lame delegation analysis

filter(lame,{_,DNSInfo=#dninfo{	domain=Domain,
				status=Status,
				nsrrs=NSRRS,
				facets=Facets}})->
	TS=esn_event_format:formatDate(calendar:local_time()),
	NSList=extractNS("|",DNSInfo),
	%% {_,F1}=extractFacetLeaf(Facets,"urn:facetmap:dns_v1"),
	F1=extractFacetLeaf(Facets,"urn:facetmap:dns_v1"),
	F=io_lib:format("~s,",[F1]),
	X=io_lib:format("~s,~s,",[TS,Domain])++F++NSList,
	{filter,X};

filter(_Type,{systimeout,{data,Domain},{limit,Limit}})->
	X=io_lib:format("~s,timeout at ~p ms",[Domain,Limit]),
	{filter,X};

filter(Type,Arg)->
	?warn({undefinedFilter,{type,Type},{arg,Arg}}),
	{filter,null}.

extractFacetLeaf(DNSInfo=#dninfo{facets=Facets},Facet)->
	extractFacetLeaf(Facets,Facet);

extractFacetLeaf({facets,Facets},Facet)->
	extractFacetLeaf(Facets,Facet);

extractFacetLeaf([{facet,Facet,[]}|Rest],Facet)->
	[];

extractFacetLeaf([{facet,Facet,Topics}|Rest],Facet) when is_list(Topics)->
	lists:last(Topics);

extractFacetLeaf([{facet,Facet,{_,Topic}}|Rest],Facet)->
	Topic;

extractFacetLeaf([{facet,AnotherFacet,Topic}|Rest],Facet)->
	extractFacetLeaf(Rest,Facet);

extractFacetLeaf([],Facet)->
	[].



filterFacet({_,DNSInfo=#dninfo{domain=Domain,facets=Facets}},FacetMapId,Topic)->
	case extractFacets(Facets,FacetMapId,Topic) of
	true->
		X=io_lib:format("~s,",[Domain]),
		{filter,X};
	_->
		{filter,null}
	end.

filterDnsFail(X={_,DNSInfo=#dninfo{}})->
	filterFacet(X,"urn:facetmap:dns_v1","dnsFail").

filterNxDomain(X={_,DNSInfo=#dninfo{}})->
	filterFacet(X,"urn:facetmap:dns_v1","nxdomain").

extractFacets({facets,Facets},FacetMap,Topic)->
	extractFacets(Facets,FacetMap,Topic);

extractFacets([{facet,FacetMap,TopicList}|_],FacetMap,Topic)->
	case lists:keysearch(Topic,2,TopicList) of
	{value,_T}->
		true;

	false->
		false
	end;

extractFacets([{facet,_FacetMap,_}|Rest],FacetMap,Topic)->
	extractFacets(Rest,FacetMap,Topic);

extractFacets([],_,_)->
	false.


filterA({_,DNSInfo=#dninfo{domain=Domain,status=Status,arrs=ARRS}})->
	AList=extractA(DNSInfo),
	X=io_lib:format("~s,",[Domain])++AList,
	{filter,X};

filterA(X)->
	?warn({filterA,nomatch,X}),
	{filter,"filterNoMatch"}.

extractA(DNSINfo=#dninfo{arrs=ARRS})->
	extractA(ARRS);

extractA({arrs,ARRS})->
	extractA(ARRS);

extractA(undefined)->
	"";

extractA(L)->
	extractA(L,[]).

extractA(X=[],L)->
	%% empty list
	L;

extractA([X={arr,A,_}|Rest],[])->
	extractA(Rest,mkip(X));

extractA([X={arr,A,_}|Rest],L)->
	extractA(Rest,L++","++mkip(X)).

mkip({arr,IP,ip4})->
	mkipv4(IP);

mkip({arr,CNAME,cname})->
        lists:flatten(io_lib:format("cname=~s",[CNAME]));


mkip(X)->
	?warn({mkip,unhandled,X}),
	"".

mkipv4({A,B,C,D}) when is_integer(A),is_integer(B),is_integer(C),is_integer(D)->
        lists:flatten(io_lib:format("~p.~p.~p.~p",[A,B,C,D]));

mkipv4(X)->
        ?warn({unhandled,{mkipv4,X}}),
	"".

filterAuthNs(#agentResult{payload=PayLoad})->
	filterAuthNs(PayLoad);

filterAuthNs({_,DNSInfo=#dninfo{domain=Domain,status=Status,nsrrs=NSRRS}})->
	NSList=extractNS(DNSInfo),
	X=io_lib:format("~s,",[Domain])++NSList,
	{filter,X};

filterAuthNs(X)->
	?warn({filterAuthNs,nomatch,X}),
	{filter,"filterNoMatch"}.

%% generate a comma delimited list of nameservers

extractNS(DNSINfo=#dninfo{nsrrs=NSRRS})->
	extractNS(",",NSRRS).

extractNS(SEP,DNSINfo=#dninfo{nsrrs=NSRRS})->
	extractNS(SEP,NSRRS);

extractNS(SEP,{nsrrs,NSRRS})->
	extractNS(SEP,NSRRS);

extractNS(_SEP,undefined)->
	"";

extractNS(SEP,L)->
	extractNS(SEP,L,[]).

extractNS(_SEP,[],L)->
	%% empty list
	L;

extractNS(SEP,[{nsrr,NS}|Rest],[])->
	extractNS(SEP,Rest,NS);

extractNS(SEP,[{nsrr,NS}|Rest],L)->
	extractNS(SEP,Rest,L++SEP++NS).

