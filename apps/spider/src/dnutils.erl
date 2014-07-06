-module(dnutils).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([
	 mkDnInfo/1,
	 mkDnsInfo/1,	%% deprecated
	 dnsCheck/1,
	 dnsCheck/2,
	 ldnsCheck/1,
	 cdnsCheck/1,
	 i_dnsCheck/1,
	 isDelegated/1,
	 isTopicMatched/2,
	 ff/2,
	 filter/2,
	 nullFacet/0,
	 multiCheckAllTiers/1,
	 multiCheckByTier/2,
	 multiCheck/1,
	 multiCheck/2
]).

-define(DOT,		"[.]").
-define(ROOT,		".").

-include("facetmap_dns_v1.hrl").
-include("domaininfo.hrl").
-include("agent.hrl").

-define(TLDS,	["com","net","org","biz","info","us"]).
-define(AU,	["com.au","net.au","org.au","id.au"]).


nullFacet()->
	?F_DNSV1_NULL.

%% filters =====================================================================

filter(Type,#agentResult{payload=PayLoad})->
	filter(Type,PayLoad);

filter(multiDNS,{Stem,Results}) when is_list(Results)->
	Fun=fun(Z)->ff(Stem,Z) end,
	{filter,format:formatList(",",Stem,Results,Fun)};

filter(Type,Results)->
	?warn({unhandledFilter,{type,Type},{results,Results}}),
	{filter,null}.


%% return how many TLDs belong to the domain (based on the last token)

getSpace(Space)->
	i_getSpace(tld:space(Space)).

i_getSpace(S=#space{})->
	S;

i_getSpace(E)->
	throw(E).

%% return the parent domain

parent(Domain)->
	%%first step is to remove any trailing "." chars
	D=stripTrailingDot(Domain),
	R=re:split(D,?DOT),
	X=lists:map(fun(Z)->binary_to_list(Z) end,R),
	i_parent(D,X).

i_parent(Domain,{ok,[Domain]})->
	?ROOT;

i_parent(_Domain,{ok,[_Head|Parent]})->
	combine(Parent);

i_parent(_Domain,[_Head|Parent])->
	combine(Parent);

i_parent(_Domain,Other)->
	throw({error,{unexpected,Other}}).

%% return the TLD of a particular domain

mkDnsInfo(HName)->
	mkDnInfo(HName).

mkDnInfo(Domain)->
	X=re:split(Domain,?DOT),
	Y=lists:map(fun(Z)->binary_to_list(Z) end,X),
	catch(i_mkDnInfo(Domain,{ok,Y})).

i_mkDnInfo(Domain,{ok,Toks}) when is_list(Toks)->
	extractSpace(Domain,Toks);

i_mkDnInfo(Domain,Toks) when is_list(Toks)->
	extractSpace(Domain,Toks);

i_mkDnInfo(_Domain,E={error,_Reason})->
	E;

i_mkDnInfo(Domain,Other)->
	throw({error,{unexpected_response,Other}}).

%% Utility functions

%% e.g 	domain.com->		["com","domain"]
%% 	domain.com.au->		["com.au","domain"]
%%	www.domain.com->	["com","domain","www"]

stripTrailingDot(Name)->
	i_stripTrailingDot(Name,lists:reverse(Name)).

i_stripTrailingDot(_,"."++RName)->
	lists:reverse(RName);

i_stripTrailingDot(Name,_RName)->
	Name.


combine(Toks) when is_list(Toks)->
%% combine the Toks and insert a DOT character between each of them

	L=lists:map(fun(Z)->Z++"." end,Toks),
	stripTrailingDot(lists:concat(L)).


extractSpace(Domain,Toks)->
	i_extractSpace(Domain,getSpace(lists:last(Toks)),lists:reverse(Toks)).

i_extractSpace(Domain,#space{space=Space,type=Class,numSegs=1},[_,Name|Subs])->
	Parent=parent(Domain),
	#domain{fqdn=Domain,
                space=Space,
                parent=Parent,
		class=Class,
                name=Name,
		rname=Name++"."++Space,
                subdomains=lists:reverse(Subs)};

i_extractSpace(Domain,#space{space=Space,type=Class,numSegs=2},[SLD,TLD,Name|Subs])->
	Parent=parent(Domain),
	#domain{fqdn=Domain,
                space=TLD++"."++SLD,
                parent=Parent,
                name=Name,
		rname=Name++"."++Space,
		class=Class,
                subdomains=lists:reverse(Subs)};

i_extractSpace(Domain,_,_)->
	throw({error,{unhandled,Domain}}).

%% DNS analysis of the domain name

mkName(Name,Space)->
	lists:append([Name,".",Space]).

%% multi yield function .. TBD need to extract this to another file

yield(Self,[],Results)->
        Results;

yield(Self,Pids,Results)->
        receive
                X={Pid,Result}->
			?dbug({yielded,X}),
                        NewPids=lists:delete(Pid,Pids),
			?dbug({waitList,NewPids}),
                        yield(Self,NewPids,lists:append([Result],Results));

                Other->
                        ?dbug({yieldUnexpected,Other})
        end.

extractName(Name)->
        %% take out any "." components
        %% that is, extract only the first token
        case utils:is_string(Name) of
        true->
                case re:split(Name,"[.]") of
                {ok,[XName|_]}->
                        XName;
                _->
                        Name
                end;
        false->
                Name
        end.

ff(Stem,{Domain,{_,Topics}})->
	case lists:member("delegated",Topics) of
	true->
		%%Tokens=string:tokens(Domain,"."),
        	%%lists:last(Tokens);
		Domain--Stem;
	_->
		[]
	end;

ff(_,{Domain,Other})->
        ?warn({unhandledFormat,Other}),
        [].

multiCheckAllTiers(Name)->
	T1=tldinfo:spacesByTier("1"),
	T2=tldinfo:spacesByTier("2"),
	T3=tldinfo:spacesByTier("3"),
	R1=lists:append(T1,T2),
	R2=lists:append(R1,T3),
	Stem=extractName(Name),
	Result=multiCheck(Stem,R2),
	?warn({resultSize,length(Result)}),
	{Stem,Result}.

multiCheckByTier(Tier,Name)->
	Stem=extractName(Name),
	Result=multiCheck(Stem,{tier,Tier}),
	{Stem,Result}.

multiCheck(Name)->
	multiCheck(Name,lists:append(?TLDS,?AU)).

multiCheck(Name,{tier,Tier})->
	multiCheck(Name,tldinfo:spacesByTier(Tier));

multiCheck(Name,Spaces) when is_list(Spaces)->
	%%?warn({multiCheck,Name,Spaces}),
	%% need to convert domain to stem only
	Domains=lists:map(fun(Z)->mkName(Name,Z) end,Spaces),
	?warn({domainList,length(Domains)}),
	F=fun(Z)->spawn(?MODULE,dnsCheck,[self(),Z]) end,
	Pids=lists:map(fun(Q)->F(Q) end,Domains),
        yield(self(),Pids,[]);

multiCheck(Name,Other)->
	{error,{badarg,Other}}.

ldnsCheck(Domain)->
	%% only allow valid domain names to be processed, otherwise
	%% throw an error
	case inet_parse:domain(Domain) of
        false->
                ?F_DNSV1_FORMATERROR;
        true->
                i_dnsCheck(Domain)
        end.

dnsCheck(Domain)->
	%% call ldnsCheck and construct the abbreviated topic heirarchy
	Result=(catch heirarchy(ldnsCheck(Domain))),
	{Domain,Result}.

dnsCheck(Pid,Domain) when is_pid(Pid)->
	%% call ldnsCheck and construct the abbreviated topic heirarchy
	?dbug({invoked,{pid,self()},{domain,Domain}}),
	Result=(catch heirarchy(ldnsCheck(Domain))),
	Pid ! {self(),{Domain,Result}}.

cdnsCheck(Domain)->
	%% call ldnsCheck and construct the complete topic heirarchy
	c_heirarchy(ldnsCheck(Domain)).

	
i_dnsCheck(#domain{fqdn=Domain})->
	i_dnsCheck(Domain);

i_dnsCheck(Domain)->
	ii_dnsCheck(Domain,ns2:getRR(nsa,Domain)).

ii_dnsCheck(Domain,{_,{error,nxdomain}})->
	?F_DNSV1_NXDOMAIN;

ii_dnsCheck(Domain,_)->
	case (catch ns2:getRR(soa,Domain)) of
	{soa,[]}->
		%% there is no SOA record but the name obviously
		%% resolves, so must be a subdomain or a wildcard
		%% 1/3/2005 - not necessarily, 
		%% we must check

		Q=mkDnsInfo(Domain),

		case mkDnsInfo(Domain) of
		#domain{subdomains=[]}->
			?F_DNSV1_ISNOTZONE;

		#domain{}->
			?F_DNSV1_ISSUBDOMAIN;

		Other->
			?F_DNSV1_OTHERDNS
		
		end;

	{soa,E={error,servfail}}->
		%% we need to check for the existence of any A
		%% records because dns also returns servfail for a
		%% wildcard delegation

		?dbug({Domain,E}),
		case ns2:getRR(a,Domain) of
		{a,ARecs} when is_list(ARecs)->
			?F_DNSV1_ISWILDCARD;
		_Other->
			?F_DNSV1_SERVFAIL
		end;

	{soa,E={error,timeout}}->
		%% we need to check for the existence of any A
		%% records because dns also returns servfail for a
		%% wildcard delegation

		?dbug({Domain,E}),
		case ns2:getRR(a,Domain) of
		{a,ARecs} when is_list(ARecs)->
			?F_DNSV1_ISWILDCARD;
		_Other->
			?F_DNSV1_TIMEOUT
		end;

	{soa,E={error,nxdomain}}->
		?dbug({Domain,E}),
		?F_DNSV1_NXDOMAIN;
		
	{soa,SOA} when is_list(SOA)->
		?F_DNSV1_ISZONE;
		
	Other->
		?warn({i_dnsCheck,Domain,{unexpected,Other}}),
		?F_DNSV1_OTHERDNS
	end.


%% filtering utilities -- customise this to implement the facet heirarchy

f([T1,_,?F_DNSV1_DNSFAIL])->
	[T1,?F_DNSV1_DNSFAIL];

f(Ts)->
	Ts.

%% generate an abbreviated form of the complete topic heirarchy

heirarchy(Topic)->
        H=heirarchy(t_parent(Topic),[Topic]),
        {{facet,?FACETMAP},lists:map(fun(Z)->{topic,element(2,Z)} end,H)}.

heirarchy(?F_DNSV1_FROOT,C)->
        C;

heirarchy(?F_DNSV1_NULL,C)->
        C;

heirarchy(Topic,Topics)->
        heirarchy(t_parent(Topic),lists:append(Topics,[Topic])).

%% generate a complete form of the topic heirarchy

c_heirarchy(Topic)->
	heirarchy(t_parent(Topic),[Topic]).

%% define all the topic parents, F_DNSV1_FROOT is at the top

t_parent(Topic) when 	Topic==?F_DNSV1_OTHERDNS;
			Topic==?F_DNSV1_SERVFAIL;
			Topic==?F_DNSV1_TIMEOUT->
	?F_DNSV1_DNSERROR;

t_parent(Topic) when	Topic==?F_DNSV1_ISZONE;
			Topic==?F_DNSV1_ISNOTZONE;
			Topic==?F_DNSV1_ISSUBDOMAIN;
			Topic==?F_DNSV1_ISWILDCARD->
	?F_DNSV1_DELEGATED;


t_parent(?F_DNSV1_FORMATERROR)->
	?F_DNSV1_ERROR;

t_parent(Topic) when	Topic==?F_DNSV1_NXDOMAIN;
			Topic==?F_DNSV1_DNSERROR->
	?F_DNSV1_DNSFAIL;


t_parent(Topic) when	Topic==?F_DNSV1_DNSFAIL;
			Topic==?F_DNSV1_DELEGATED;
			Topic==?F_DNSV1_ERROR->
	?F_DNSV1_FROOT;

t_parent(Other)->
	?warn({t_parent,{no_parent_facet,?FACETMAP,Other}}),
	?F_DNSV1_NULL.

%% helper routines

isTopicMatched(Topic,TopicList)->
	lists:member(Topic,TopicList).

isDelegated(Domain)->
	isDelegated(Domain,cdnsCheck(Domain)).

isDelegated(_Domain,TopicList)->
	isTopicMatched(?F_DNSV1_DELEGATED,TopicList).	

