%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id: inet_dns.hrl,v 1.1.1.1 2008/06/18 00:56:57 thobbins Exp $
%%
%%
%% Defintion for Domain Name System
%%

%%
%% Currently defined opcodes
%%
-define(QUERY,    16#0).          %% standard query
-define(IQUERY,   16#1).	      %% inverse query 
-define(STATUS,   16#2).	      %% nameserver status query 
%% -define(xxx,   16#3)  %% 16#3 reserved
%%  non standard
-define(UPDATEA,  16#9).	       %% add resource record
-define(UPDATED,  16#a).	       %% delete a specific resource record
-define(UPDATEDA, 16#b).	       %% delete all nemed resource record
-define(UPDATEM,  16#c).	       %% modify a specific resource record
-define(UPDATEMA, 16#d).	       %% modify all named resource record

-define(ZONEINIT, 16#e).	       %% initial zone transfer 
-define(ZONEREF,  16#f).	       %% incremental zone referesh


%%
%% Currently defined response codes
%%
-define(NOERROR,  0).		%% no error
-define(FORMERR,  1).		%% format error
-define(SERVFAIL, 2).		%% server failure
-define(NXDOMAIN, 3).		%% non existent domain
-define(NOTIMP,	  4).		%% not implemented
-define(REFUSED,  5).		%% query refused
%%	non standard 
-define(NOCHANGE, 16#f).		%% update failed to change db


%%
%% Type values for resources and queries
%%
-define(T_A,		1).		%% host address
-define(T_NS,		2).		%% authoritative server
-define(T_MD,		3).		%% mail destination
-define(T_MF,		4).		%% mail forwarder
-define(T_CNAME,	5).		%% connonical name
-define(T_SOA,		6).		%% start of authority zone
-define(T_MB,		7).		%% mailbox domain name
-define(T_MG,		8).		%% mail group member
-define(T_MR,		9).		%% mail rename name
-define(T_NULL,		10).		%% null resource record
-define(T_WKS,		11).		%% well known service
-define(T_PTR,		12).		%% domain name pointer
-define(T_HINFO,	13).		%% host information
-define(T_MINFO,	14).		%% mailbox information
-define(T_MX,		15).		%% mail routing information
-define(T_TXT,		16).		%% text strings
-define(T_AAAA,         28).            %% ipv6 address
%% SRV (RFC 2052)
-define(T_SRV,          33).            %% services
%%      non standard
-define(T_UINFO,	100).		%% user (finger) information
-define(T_UID,		101).		%% user ID
-define(T_GID,		102).		%% group ID
-define(T_UNSPEC,	103).		%% Unspecified format (binary data)
%%	Query type values which do not appear in resource records
-define(T_AXFR,		252).		%% transfer zone of authority
-define(T_MAILB,	253).		%% transfer mailbox records
-define(T_MAILA,	254).		%% transfer mail agent records
-define(T_ANY,		255).		%% wildcard match

%%
%% Symbolic Type values for resources and queries
%%
-define(S_A,		a).		%% host address
-define(S_NS,		ns).		%% authoritative server
-define(S_MD,		md).		%% mail destination
-define(S_MF,		mf).		%% mail forwarder
-define(S_CNAME,	cname).		%% connonical name
-define(S_SOA,		soa).		%% start of authority zone
-define(S_MB,		mb).		%% mailbox domain name
-define(S_MG,		mg).		%% mail group member
-define(S_MR,		mr).		%% mail rename name
-define(S_NULL,		null).		%% null resource record
-define(S_WKS,		wks).		%% well known service
-define(S_PTR,		ptr).		%% domain name pointer
-define(S_HINFO,	hinfo).		%% host information
-define(S_MINFO,	minfo).		%% mailbox information
-define(S_MX,		mx).		%% mail routing information
-define(S_TXT,		txt).		%% text strings
-define(S_AAAA,         aaaa).          %% ipv6 address
%% SRV (RFC 2052)
-define(S_SRV,          srv).           %% services
%%      non standard
-define(S_UINFO,	uinfo).		%% user (finger) information
-define(S_UID,		uid).		%% user ID
-define(S_GID,		gid).		%% group ID
-define(S_UNSPEC,	unspec).        %% Unspecified format (binary data)
%%	Query type values which do not appear in resource records
-define(S_AXFR,		axfr).		%% transfer zone of authority
-define(S_MAILB,	mailb).		%% transfer mailbox records
-define(S_MAILA,	maila).		%% transfer mail agent records
-define(S_ANY,		any).		%% wildcard match

%%
%% Values for class field
%%

-define(C_IN,		1).      	%% the arpa internet
-define(C_CHAOS,	3).		%% for chaos net at MIT
-define(C_HS,		4).		%% for Hesiod name server at MIT
%%  Query class values which do not appear in resource records
-define(C_ANY,		255).		%% wildcard match 


%% indirection mask for compressed domain names
-define(INDIR_MASK, 16#c0).

%%
%% Structure for query header, the order of the fields is machine and
%% compiler dependent, in our case, the bits within a byte are assignd
%% least significant first, while the order of transmition is most
%% significant first.  This requires a somewhat confusing rearrangement.
%%
-record(dns_header, 
	{
	 id = 0,       %% ushort query identification number 
	 %% byte F0
	 qr = 0,       %% :1   response flag
	 opcode = 0,   %% :4   purpose of message
	 aa = 0,       %% :1   authoritive answer
	 tc = 0,       %% :1   truncated message
	 rd = 0,       %% :1   recursion desired 
	 %% byte F1
	 ra = 0,       %% :1   recursion available
	 pr = 0,       %% :1   primary server required (non standard)
	               %% :2   unused bits
	 rcode = 0     %% :4   response code
	}).

-record(dns_rec,
	{
	 header,       %% dns_header record
	 qdlist = [],  %% list of question entries
	 anlist = [],  %% list of answer entries
	 nslist = [],  %% list of authority entries
	 arlist = []   %% list of resource entries
	}).

%% DNS resource record
-record(dns_rr,
	{
	 domain,        %% resource domain
	 class,         %% reource class (atom i.e c_in)
	 type,          %% resource type (atom i.e t_any)
	 cnt = 0,       %% access count
	 tm,            %% creation time
	 ttl,           %% time to live
         bm = [],       %% Bitmap storing domain character case information.
	 data = [],     %% raw data
         func = false   %% Optional function calculating the data field.
	}).

-record(dns_query,
	{
	 domain,     %% query domain
	 type,       %% query type
	 class       %% query class
	 }).