-module(ns2).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export(	[
		 authNs/1,
		 getRR/1,
		 getRR/2,
		 getRR2/1	%% includes authoritative nameserver
		 ]).


-define(ALL_RRTYPES,		[soa,mx,a,aaaa,ns,srv]).

-include("schema.hrl").
-include_lib("kernel/src/inet_dns.hrl").
-include("monitor.hrl").
-sysloglevel(?TRACE_WARN).

-include("domaininfo.hrl").	%% added 18/8/2005

-define(NSCOUNT,	3).	%% max number of auth NS to try

authNs(Domain)->
	case dnutils:mkDnsInfo(Domain) of
	#domain{space=Space}->
		{ns,R}=getRR(ns,Space),
		?info({authNs,{domain,Domain},{space,Space},{ns,R}}),
		case is_list(R) of
		true->
			NS=R,
			NSAddrs=lists:map(fun(Z)->getRR(a,Z) end,NS),
			{ns,catch authNs(Domain,NSAddrs,0,[])};
		_->
			{ns,R}
		end;
	Error->
		%% Error
		{ns,Error}
	end.


%%authNs(Domain)->
%%	D=#domain{space=Space}=dnutils:mkDnsInfo(Domain),
%%	{ns,NS}=getRR(ns,Space),
%%	NSAddrs=lists:map(fun(Z)->getRR(a,Z) end,NS),
%%	catch authNs(Domain,NSAddrs,0,[]).

authNs(Domain,_,?NSCOUNT+1,Result)->
	?info({reachedMaxCount,{domain,Domain},{maxCount,?NSCOUNT}}),
	Result;

authNs(Domain,[{a,[IP]}|Rest],Count,_LastResult)->
	NSIP=[{IP,53}],	%% IP address of nameserver
	%% R=x_inet_res:nnslookup(Domain,1,ns,NSIP),
	R=inet_res:nnslookup(Domain,1,ns,NSIP),
	case parseNS(delegated,R) of
	E={error,Reason}->
		?dbug({gotError,Domain,IP,Reason}),
		authNs(Domain,Rest,Count+1,E);

	[]->
		?dbug({gotEmptyResult,Domain,IP}),
		authNs(Domain,Rest,Count+1,[]);
	
	Result->
		throw(Result)
	end;


authNs(_Domain,[],_Count,Result)->
	Result.
	

getRR2(Name)->
	%% remove ns from ?ALL_RRTYPES
	RRs=lists:delete(ns,?ALL_RRTYPES),
	XRRs=lists:append([RRs,[nsa]]),
	getRR(XRRs,Name).


getRR(Name)->
	getRR(?ALL_RRTYPES,Name).

getRR(RRType,Name) when is_list(RRType)->
	lists:map(fun(Z)->getRR(Z,Name) end,RRType);

%% CAUTION: There is a situation here which causes a potential problem
%% which should be caught! 
%% if a getRR(a,Domain) returns {a,[Domain]} this indicates that the DNS
%% has not been configured properly and that there is a single CNAME pointing
%% to itself

getRR(nsa,Name)->
	authNs(Name);

getRR(RRType,Name)->
	Result=inet_res:resolve(Name,in,RRType),
	case Result of
	R->
		Z=parseNS(an,R),
		{RRType,Z}
	end.

parseNS(T,{ok,Rec}) when is_record(Rec,dns_rec)->
	pNS(T,Rec);

parseNS(_T,Other)->
	Other.

pNS(delegated,Rec) when Rec#dns_rec.anlist==[]->
	pNS(ns,Rec);

pNS(delegated,Rec)->
	pNS(an,Rec);

pNS(ar,Rec)->
	AR=Rec#dns_rec.arlist,
	lists:map(fun(Z)->formatNs(ar,Z) end,AR);

pNS(ns,Rec)->
	NS=Rec#dns_rec.nslist,
	lists:map(fun(Z)->formatNs(ns,Z) end,NS);

pNS(an,Rec)->
	AN=Rec#dns_rec.anlist,
	lists:map(fun(Z)->formatNs(an,Z) end,AN);
	
pNS(_Z,Other)->
	Other.

formatNs(ar,Z) when is_record(Z,dns_rr)->
	{Z#dns_rr.data,53};

formatNs(_,Z) when is_record(Z,dns_rr)->
	Z#dns_rr.data.
