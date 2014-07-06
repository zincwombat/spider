-define(DOMAIN_V1_URI,		"urn:domain").

-undef(THIS_VERSION).
-define(THIS_VERSION,   #version{version=?DOMAIN_V1_URI}).

-record(run_request,
		{
		 threads=undefined,	%% number of threads to use
		 async=true,		%% true|false
		 filter=new,		%% new|redo|dnsfail ... etc
		 type=all		%% delegated, all .....
		}).

-record(result_set,
		 {version=?THIS_VERSION,
		  count=0,
		  filter,
		  results=[]}).

-record(domain_status,	
		{version=?THIS_VERSION,
		 domain_name=[],
		 status=[],
		 delegation_status=[],
		 soa=[],
		 ns=[],
		 a=[],
		 mx=[],
		 web_status=[],
		 mail_status=[],
		 protocols=[],
		 filter_set=[],
		 resolver=[],
		 filter=[],
		 time_stamp=[]}).

-record(domain_filter_set,
		{version=?THIS_VERSION,
		 domain_name=[],
		 filter_set=[]}).

-record(domain_filter,
		{version=?THIS_VERSION,
                 name,
                 value}).

-record(mail_result,{   version=?THIS_VERSION,
                        domain,
                        mailserver,
                        result,
                        time_stamp,
                        resolver,
                        status}).

-record(domain_name,
		{version=?THIS_VERSION,
		 name}).

-record(domain_list,
		{version=?THIS_VERSION,
		 filter,
		 count,
		 next,
		 domain_list=[]}).

-record(delegation_status,
		{version=?THIS_VERSION,
		 status}).

-record(soa,
		{version=?THIS_VERSION,
		 rrs}).

-record(a,
		{version=?THIS_VERSION,
		 name,
		 rrs}).

-record(mx,
		{version=?THIS_VERSION,
		 name,
		 rrs}).

-record(ns,
		{version=?THIS_VERSION,
		 rrs}).

-record(nsrr,
		{version=?THIS_VERSION,
		 ns}).

-record(ip,
		{version=?THIS_VERSION,
		 ip}).

-record(mxrr,
		{version=?THIS_VERSION,
		 prio,
		 server}).

-record(time_stamp,
		{version=?THIS_VERSION,
		 time}).

-record(web_status,
		{version=?THIS_VERSION,
		 name,
		 status}).

-record(web_result,
		{version=?THIS_VERSION,
		 response=[],
		 protocol=[],
		 host=[],
		 port=[],
		 path=[]}).

-record(x_protocol,
		{version=?THIS_VERSION,
		 domain,
		 name,
		 port,
		 status}).

-record(protocol_result,
		{version=?THIS_VERSION,
                 result}).

-record(http_response,
		{version=?THIS_VERSION,
                 response}).

-record(protocol,
		{version=?THIS_VERSION,
                 protocol}).

-record(host,
		{version=?THIS_VERSION,
                 host}).

-record(port,
		{version=?THIS_VERSION,
                 port}).

-record(path,
		{version=?THIS_VERSION,
                 path}).

-record(error,	
		{version=?THIS_VERSION,
		 code,
		 location,
                 string}).

-record(engine,
		{version=?THIS_VERSION,
		 engineID,
		 runstate,
		 res_ns,
		 alt_res_ns,
		 cache}).

-record(cache,
		{version=?THIS_VERSION,
		 domains,
		 currentDomain,
		 memory,
		 filter,
		 loadfile,
		 starttime,
		 endtime,
		 maxthreads,
		 curthreads,
		 requests,
		 currentStats,
		 history=[]}).

-record(current,
		{version=?THIS_VERSION,
		 timestamp,
		 statsrec}).

-record(history,
		{version=?THIS_VERSION,
		 timestamp,
		 action,
		 statsrec}).

-record(action,
		{version=?THIS_VERSION,
		 operation,
		 action}).

-record(action_detail,
		{version=?THIS_VERSION,
		 action_detail}).

-record(statsrec,
		{version=?THIS_VERSION,
		 any,
		 new,
		 redo,
		 nxd,
		 tmout,
		 srvfail,
		 webok,
		 weberr,
		 redir,
		 nohost,
		 mxok,
		 nomail}).

-record(filter_set,
		{version=?THIS_VERSION,
		 filtername,
		 results=[]}).

-record(filter_count,
		{version=?THIS_VERSION,
                 filter,
                 count}).
		 

-record(mstatus,

		{domains,	%% ets table size
		 runstate,	%% runstate
		 loadfile,	%% loadfile
		 start_time,	%% run start time
		 end_time,	%% run end time
		 current,	%% current domain being processed
		 cur_threads,	%% current active threads
		 max_threads,	%% max threads
		 dgetrqs,	%% cumulative dget requests 
		 filter,	%% current filter
		 memory,	%% current memory being used
		 res_ns,	%% primary ns
		 alt_res_ns	%% alternate ns
		}).

-record(dstatus,
		{
		status,		%% undelegated / delegated
		domain,		%% domain name
		soa,		%% SOA RRs
		ns,		%% Authoritative NS
		a,		%% DNS A RRs
		www_a,		%% DNS A records for www.domain
		mx,		%% DNS MX records
		webcheck,	%% webcheck results domain
		wwebcheck,	%% webcheck results for www.domain
		mailcheck,	%% mailcheck results from check
		resolver,	%% resolving nameserver
		timestamp	%% timestamp of last analysis
		}
).


-define(FILTERS,                [any,
                                 new,
                                 redo,
                                 nxd,           %%nxdomain,
                                 tmout,         %%timeout,
				 srvfail,	
				 %%formerr,
				 nohost,
				 resolve,
                                 webok,
				 webfail,
                                 mailok,
                                 mailerr]).

-define(EXP_FILTERS,            [nxd,
                                 srvfail,
                                 tmout,
                                 webok,
                                 redir,
                                 weberr,
                                 nohost,
                                 mxok,
                                 nomail]).

-define(IN_USE,			[inuse2]).

-define(GLOBAL,			[any,new,redo]).

-define(ORTHOGONAL,		[
				 analysed,
				 dnsfail,
				 nxd,
				 dnserr,
				 tmout,
				 srvfail,
				 unknown,
				 formerr,
				 dnsok,
				 host,
				 nohost,
				 noweb,
				 web,
				 redir,
				 webok,
				 weberr,
				 mxok,
				 nomail
				]).


%%-define(CONTROL,		[any,
%%				 new,
%%				 redo,
%%				 dnserr,
%%				 notpositive]).


-define(BUSINESS_FILTERS,	[dnsfail,
				 inuse,
				 web,
				 webonly,
				 mailonly,
				 mailandweb,
				 nomailnoweb]).

-define(FILTER_TYPES,		[set1,
				 set2,
				 business,
				 control,
				 othogonal]).

-define(MAIL_ONLY,		[resolve,
				 nomail,
				 mailok,
				 mailerr,
				 mxserr,
				 mxoerr]).


-define(WEB_ONLY,		[
                                 nohttp,
                                 http,
                                 '2NN',
                                 '3NN',
                                 '0NN',
                                 '1NN',
                                 '4NN',
                                 '5NN',
                                 'NNN']).

-define(CONTROL,		?GLOBAL++?ORTHOGONAL++?WEB_ONLY).

-define(WEB_FILTERS,		?GLOBAL++?WEB_ONLY).
-define(MAIL_FILTERS,		?GLOBAL++ [dnsfail] ++ ?MAIL_ONLY).
