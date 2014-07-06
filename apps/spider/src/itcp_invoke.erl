-module(itcp_invoke).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([
		request/3,
		request/4
	 ]).


-define(TCP_CONNECT_TMOUT,30000).	%% 30 second timeout
-define(MAX_CNAME_LOOP,5).		%% max DNS CNAME lookups
-define(DEFAULT_TRACELEVEL,4).


%% Fun takes as an argument a Socket reference to a tcp connection

request(Domain,Port,Fun,TraceLevel)->
	request1(Domain,Port,Fun,0).

request(Domain,Port,Fun)->
	request1(Domain,Port,Fun,0).

request1(Domain,Port,Fun,?MAX_CNAME_LOOP)->
	?warn({cname_loop_max,Domain,?MAX_CNAME_LOOP}),
	throw({error,cnameloop});

%% we need also to accommodate a Domain IP address and figure out a 
%% guard for this case

request1(Address,Port,Fun,Cname_Cnt)->
	%% domains and IP addresses qualify here
	%% first check to see if Address is actually a Domain
	?dbug({request,Address,Port,Fun,Cname_Cnt}),
	case inet_parse:domain(Address) of
	true->
		?dbug({isAddress,Address}),
		case (catch ns2:getRR(a,Address)) of
		X={a,E={error,Reason}}->
			?dbug({nslookupError,Address,E}),
			%% e.g. nxdomain, srvfail etc
			{error,{dnserr,Reason}};

		X={a,[]}->
			%% seems to be valid
			?dbug({nslookupA,X}),
			{error,no_a};

		X={a,RRs} when is_list(RRs)->
			%% seems to be valid
			?dbug({nslookupA,X}),
			i_request(RRs,Address,Port,[],Fun,Cname_Cnt);

		Other->
			?warn({nslookupError,Address,Other}),
			{error,{dns,Other}}
		end;
	_->
		%% maybe its an IP address ?
		?dbug({isNotAddress,Address}),
		case inet_parse:address(Address) of
		{ok,IP}->
			?dbug({isIpAddress,IP}),
			i_request([IP],Address,Port,[],Fun,Cname_Cnt);
		EE->
			?warn({badarg,Address,EE}),
			{error,{badarg,{Address,EE}}}
		end
	end;

request1(Other,_,_,_)->
	?warn({badarg,Other}),
	{error,{badarg,Other}}.

i_request([],Domain,Port,[],Fun,Cname_Cnt)->
	%% no A records
	?dbug({i_request,noARecords,Domain}),
	{error,no_a};

i_request([],Domain,Port,LastError,Fun,Cname_Cnt)->
	%% no A records, return the last error encountered
	?dbug({i_request,noMoreARecords,Domain,{lastError,LastError}}),
	LastError;

i_request([IP|Rest],Domain,Port,LastError,Fun,Cname_Cnt) when is_tuple(IP)->
	%% not a CNAME, assume a valid IP address
	%% pass this IP address to the Fun
	%% DO WE NEED TO CATCH THIS ????
	?dbug({i_request,Domain,{thisIp,IP},{port,Port},{ipRest,Rest}}),
	case catch gen_tcp:connect(IP,
                                   Port,
                                   [binary,{packet,0},{keepalive,true}],
                                   ?TCP_CONNECT_TMOUT) of
        {ok,Socket}->
		?dbug({i_request,Domain,{thisIp,IP},{port,Port},connected}),
                case Fun(Socket) of
		E={error,Reason}->
			?dbug({funError,Domain,{ip,IP},E}),
			%% optionally we could crash here and not try
			%% additional IP addresses
			i_request(Rest,Domain,Port,E,Fun,0);

		Other->
			?dbug({funNoError,Domain,{ip,IP},Other}),
			Other
		end;
		

        EE={'EXIT',Reason}->
		?warn({i_request,connectFailed,Domain,{thisIp,IP},EE}),
                E={error,{tcp_connect,Reason}},
		i_request(Rest,Domain,Port,E,Fun,0);

        E={error,timeout}->
		Error={error,{tcp_connect,{timeout,?TCP_CONNECT_TMOUT}}},
		?info({i_request,connectFailed,Domain,{thisIp,IP},Error}),
                i_request(Rest,Domain,Port,Error,Fun,0);

        E={error,Reason}->
		Error={error,{tcp_connect,Reason}},
		?info({i_request,connectFailed,Domain,{thisIp,IP},Error}),
                i_request(Rest,Domain,Port,Error,Fun,0);

        Other->
		Error={error,{tcp_connect,Other}},
		?warn({i_request,connectFailed,Domain,{thisIp,IP},Other}),
                i_request(Rest,Domain,Port,Error,Fun,0)
        end;


i_request([CNAME|Rest],Domain,Port,LastError,Fun,Cname_Cnt) when is_list(CNAME),
			is_integer(hd(CNAME))->
	%% is a CNAME, each time we encounter a CNAME we increment the
	%% counter and recurse
	?dbug({i_request,isCname,{cname,CNAME},{domain,Domain}}),
	CN=Cname_Cnt+1,
	case request1(CNAME,Port,Fun,CN) of
	E={error,Reason}->
		i_request(Rest,Domain,Port,E,Fun,CN);

	Other->
		Other
	end.

