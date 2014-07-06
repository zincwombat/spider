-define(ANALYSIS_V1_URI,	'urn:analysis_v1').

-record(analysis_opt,{	key,
			module,
		 	function,
			desc}).

-define(SPIDER,	#analysis_opt{
			key=1,
			module=spider,
			function=analyse,
			desc="Web Crawler designed to gather data about a web site"}).

-define(DNS,	#analysis_opt{
			key=2,
			module=urn_domain,
			function=dnsCheck,
			desc="DNS Analysis"}).

-define(NS,	#analysis_opt{
			key=3,
			module=urn_domain,
			function=delegationCheck,
			desc="DNS Analysis - Authoritative Nameservers"}).

-define(ALL,	#analysis_opt{
			key=4,
			module=urn_domain,
			function=allCheck,
			desc="Domain Analysis - DNS, Web and Email"}).

-define(TEST,	#analysis_opt{
			key=5,
			module=urn_domain,
			function=check_all,
			desc="Modified  Domain Analysis - DNS, Web and Email"}).

-define(ANALYSIS,	[?SPIDER,?DNS,?NS,?ALL,?TEST]).

%%-define(ANALYSIS,	[]).
