-module(imail).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([session/1,
	 mcheck/1,
	 nullFacet/1,
	 test/0]).


-include("facetmap_mail_v1.hrl").
-sysloglevel(?TRACE_WARN).

-define(SMTP_PORT,	25).
-define(SMTP_TMOUT,15000).
-define(HELO,	"EHLO melbourneit.com.au\r\n").
-define(QUIT,	"QUIT\r\n").

nullFacet(Domain)->
	{Domain,heirarchy(?ERROR)}.

mcheck(Domain)->
	Result=
	case ns2:getRR(mx,Domain) of
	{mx,[]}->
		%% no MX records, check for an SMTP server
		%% listening directly at domain
		?dbug({noMxRecords,Domain}),
		d_session({1,Domain});

	{mx,MX} when is_list(MX)->
		?dbug({mxRecords,Domain,MX}),
		ValidMX=lists:filter(fun(Z)->is_mx(Z) end,MX),
		session(ValidMX);

	{mx,{error,Reason}}->
		?DNSERROR;

	Other->
		?warn({unexpected,Domain,Other}),
		?ERROR
	end,
	{Domain,heirarchy(Result)}.

is_mx(Z={Prio,Server})->
        true;

is_mx(Z)->
        false.


test()->
	session([{1,"m1.dnsix.com"}]).

d_session({_Prio,Domain})->

	%% check whether the domain has an SMTP server listening
	%% i.e. there are no MX records

	?dbug({dsession,Domain}),
	Reply=handle_session(Domain),
	case t_parent(Reply) of
	?NOMAIL->
		?NODIRECTSMTP;
	Other->
		%% we have mail .....
		Reply
	end.

	
session([])->
	%% expect a list of tuples of the form {Prio,MailServer}
	%% where Prio is the MX record priority and MailServer
	%% is the MailServer domain (or IP address)
	?dbug({session,no_mx}),
	?NOSMTP;

session(Q=[{_,MailServer}])->

	?dbug({session,{lastMx,Q}}),

	%% logic should be - check the cache first for an entry
	%% for MailServer

	case mcache:get(MailServer) of
	[]->
		%% either the MailServer is not in the cache,
		%% or the mcache server is not running

		?dbug({session,cacheMiss,MailServer}),

		Result=handle_session(MailServer),
		mcache:set(MailServer,Result),
		Result;

	Status->
		?dbug({session,cacheHit,MailServer,Status}),
		Status
		
	end;

session(Q=[M1={_,MailServer}|Rest])->

	?dbug({session,{nonLastMx,M1},{rest,Rest}}),

	%% logic should be - check the cache first for an entry
	%% for MailServer

	case mcache:get(MailServer) of
	[]->
		%% either the MailServer is not in the cache,
                %% or the mcache server is not running

		?dbug({session,cacheMiss,MailServer}),
		
		Result=handle_session(MailServer),
		mcache:set(MailServer,Result),
                Result;

	Status->
		?dbug({session,cacheHit,MailServer,Status}),
		Status

	end.

i_session(MailServer,Socket)->
	%% Send msg and receive response

	?dbug({i_session,{mailServer,MailServer}}),
	Reply=mreceive(Socket,20000),
	gen_tcp:close(Socket),
	mdecode(Reply).

handle_session(MailServer)->

	F=fun(Z)->i_session(MailServer,Z) end,

	I=(catch itcp_invoke:request(MailServer,?SMTP_PORT,F)),

	?info({itcpInvokeReturn,I,MailServer}),

	Result=
	case I of
	R={ok,Code} when is_integer(Code)->
		?MKTOPIC(Code);

	{error,{dnserr,DNSError}} when is_atom(DNSError)->
               	?DNSERROR;

       	{error,no_a}->
               	?NOARECS;
	
	%% any tcp_connect errors result from a failure of
	%% gen_tcp:connect()

       	{error,{tcp_connect,econnrefused}}->
		%% assume this means no smtp server
               	?ECONNREFUSED;

       	{error,{tcp_connect,{timeout,Timeout}}}->
		%% assume this means no smtp server
               	?TCPERROR;

       	{error,{tcp_connect,Reason}}->
		%% assume this means no smtp server
               	?TCPERROR;

	%% any tcp_error returns...result from errors AFTER the
	%% socket has been successfully connected and during the
	%% read from the socket

       	{error,{tcp_error,Reason}}->
               	?SMTPERROR;

       	{error,{socket,Reason}}->
		%% unknown error during tcp read
               	?SMTPERROR;

       	{error,{mailrcv,timeout}}->
		%% timeout during tcp read
               	?SMTPERROR;

       	{error,{badarg,Reason}}->
               	?BADARG;

	{error,{smtpError,ErrorCode}}->
		?MKTOPIC(ErrorCode);

	Other->
		?warn({session,{unhandled,Other}}),
			?ERROR
	end.

mreceive(Socket,TimeoutDelay)->
	%% if we are here, we have connected to the SMTP server, so assume
	%% that errors correspond to an SMTPERROR
        receive
        {tcp,Socket,B}->
		R=binary_to_list(B),
		?dbug({mreceive,R}),
		R;

        {tcp_closed,Socket}->
		?dbug({mreceive,tcp_closed}),
            	{error,{tcp_error,socket_closed_in_read}};

        {tcp_error,Socket,Reason}->
		?dbug({mreceive,tcp_error,Reason}),
            	{error,{tcp_error,Reason}};

        Other->
		?dbug({mreceive,other,Other}),
            	{error,{socket,Other}}

    after TimeoutDelay->
		?dbug({mreceive,timeout,TimeoutDelay}),
            	{error,{mailrcv,timeout}}
    end.

mdecode(RCode) when is_list(RCode)->
	SMTPCode=string:substr(RCode,1,3),
	Code=
	case catch list_to_integer(SMTPCode) of
	{'EXIT',Reason}->
		500;
	I->
		I
	end,
	{ok,Code};

mdecode(Error)->
	Error.

%% this should be a generic filter based on the facetmap

f([T1,T2,?MAIL])->
        [T1,?MAIL];

f(Ts)->
        Ts.


heirarchy(Topic)->
	H=heirarchy(t_parent(Topic),[Topic]),
	?dbug({heirarchy,H}),
        {{facet,?FACETMAP},lists:map(fun(Z)->{topic,element(2,Z)} end,H)}.

heirarchy(?FROOT,C)->
        C;

heirarchy(?NULL,C)->
        C;

heirarchy(Topic,Topics)->
        heirarchy(t_parent(Topic),lists:append(Topics,[Topic])).

t_parent(Topic) when    Topic==?BADARG;
                        Topic==?OTHERERROR->
        ?ERROR;

t_parent(Topic) when    Topic==?DNSERROR;
                        Topic==?NOARECS;
                        Topic==?TCPERROR;
                        Topic==?NOSMTP;
			Topic==?NOMXRECS->
        ?NOMAIL;

t_parent(Topic) when	Topic==?ECONNREFUSED->
	?NOSMTP;

t_parent(Topic) when	Topic==?SMTPERROR->
	?NOSMTP;

t_parent(Topic) when    Topic==?MAIL;
                        Topic==?NOMAIL;
                        Topic==?ERROR->
        ?FROOT;

t_parent(Topic) when	Topic==?NODIRECTSMTP->
	?NOMXRECS;

t_parent({?FACETMAP,"smtp1"++Rest})->
        ?SMTP;
t_parent({?FACETMAP,"smtp2"++Rest})->
        ?SMTP;
t_parent({?FACETMAP,"smtp3"++Rest})->
        ?SMTP;
t_parent({?FACETMAP,"smtp4"++Rest})->
        ?SMTP;
t_parent({?FACETMAP,"smtp5"++Rest})->
        ?SMTP;

t_parent(Topic) when	Topic==?SMTP->
	?MAIL;

t_parent(Other)->
        ?warn({no_parent,Other}),
        ?NULL.

