-include("xmlinfo.hrl").

-define(URN_DOMAIN,		"urn:domain:v1").
-define(URN_DOMAIN_PFX,		"d1").
-define(XML_URN_DOMAIN(TYPE),	xmlinfo=?XML(?URN_DOMAIN,TYPE,?URN_DOMAIN_PFX)).
-undef(THIS_XMLNS).
-define(THIS_XMLNS,		xmlinfo=?XML(?URN_DOMAIN)).

-record(facets,		{facets=[]}).
-record(soa,		{master,email,serial,refresh,retry,expires,ttl}).
-record(mxrrs,		{mxrrs}).
-record(arrs,		{arrs}).
-record(nsrrs,		{nsrrs}).
-record(cnamerrs,	{cnamerrs}).
-record(isps,		{isps}).

-record(dninfo,		{agent_id,
			 agent_timeout,
			 domain,
			 tld,
			 basedn_status,
			 basedn,
			 basedn_delegations,
			 isCname,
			 tm_start,
			 tm_stop,
			 status,
			 cstatus,
			 timestamp,
			 soa=#soa{},
			 cnamerrs=#cnamerrs{},
			 nsrrs=#nsrrs{},
			 mxrrs=#mxrrs{},
			 arrs=#arrs{},
			 isps=#isps{},
			 facets=#facets{},
			 checks=[]
			}).

-record(facet,		{facet,topics}).
-record(topic,		{topic}).
-record(hstatus,	{
				fqdn,
				fqdn_status,
				basedn,
				basedn_status,
				isCname
			}).
-record(cstatus,	{slist=[]}).
-record(master,		{master}).
-record(email,		{email}).
-record(serial,		{serial}).
-record(refresh,	{refresh}).
-record(retry,		{retry}).
-record(expires,	{expires}).
-record(ttl,		{ttl}).
-record(status,		{status}).
-record(arr,		{addr,type}).
-record(mxrr,		{server,prio}).
-record(ip4address,	{ip4address}).
-record(cname,		{cname}).
-record(nsrr,		{server}).
-record(prio,		{prio}).
-record(iptype,		{iptype}).
-record(server,		{server}).

-record(domain_info,	{?THIS_XMLNS,
			domain,
			status,
			timestamp,
			soa,
			mxrrs,
			arrs,
			nsrrs,
			facets}).

-record(soa_info,	{?THIS_XMLNS,
			master,
			email,
			serial,
			refresh,
			retry,
			expires,
			ttl}).


-undef(THIS_XMLNS).
