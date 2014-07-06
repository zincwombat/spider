-module(facetmap_web_v1).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").
-export([classify/1,
	 xclassify/1,
	 nullFacet/0]).

-include("facetmap_web_v1.hrl").

xclassify(Arg)->
	C=classify(Arg),
	heirarchy(C).

nullFacet()->
		?NULL.

classify(null)->
		?NULL;

classify(nxdomain)->
		?NXDOMAIN;

classify({ok,{{"HTTP/"++_Rest,Code,Status},Headers,_Body}}) when is_integer(Code)->
		http_classify(Code,Status,Headers);

classify({error,{failed_connect,[{to_address,_},{inet,[inet],nxdomain}]}})->
		?NXDOMAIN;

classify({error,{failed_connect,[{to_address,_},{inet,[inet],etimedout}]}})->
		?DNSERROR;

classify({error,{failed_connect,[{to_address,_},{inet,[inet],econnrefused}]}})->
		?ECONNREFUSED;

classify({error,{failed_connect,[{to_address,_},{inet,[inet],_Other}]}})->
		?HTTPERROR;

classify({ok,Code}) when is_integer(Code)->
		?MKTOPIC(Code);

classify({redirect,RURL})->
		?HTTPREDIRECT;

classify({error,{dnserr,DNSError}}) when is_atom(DNSError)->
		?DNSERROR;

classify({error,no_a})->
		?NOARECS;

classify({error,econnrefused})->
		?ECONNREFUSED;

classify({error,socket_closed_remotely})->
		?ECONNREFUSED;

classify({error,{could_not_parse_as_http,_}})->
		?HTTPERROR;

classify({error,{tcp_connect,{timeout,Timeout}}})->
		?TCPERROR;

classify({error,{tcp_connect,Reason}})->
		?TCPERROR;

classify({error,{http,Reason}})->
		?HTTPERROR;

classify({error,{badarg,Reason}})->
		?BADARG;

classify(Other)->
		?warn({noClassification,Other}),
		?BADARG.

http_classify(Code,Status,Headers)->
	?MKTOPIC(Code).

%% Topic analysis

f([T1,T2,?WEB])->
	[T1,?WEB];

f(Ts)->
	Ts.

heirarchy(Topic)->
	H=heirarchy(t_parent(Topic),[Topic]),
        {{facet,?FACETMAP},lists:map(fun(Z)->{topic,element(2,Z)} end,H)}.

heirarchy(?FROOT,C)->
	C;

heirarchy(?NULL,C)->
	C;

heirarchy(Topic,Topics)->
	heirarchy(t_parent(Topic),lists:append(Topics,[Topic])).

t_parent(Topic) when	Topic==?BADARG;
			Topic==?OTHERERROR->
	?ERROR;

t_parent(Topic)	when	Topic==?DNSERROR;
			Topic==?NOARECS;
			Topic==?NXDOMAIN;
			Topic==?TCPERROR;
			Topic==?ECONNREFUSED;
			Topic==?NOHTTP->
	?NOWEB;

t_parent(Topic) when	Topic==?HTTPCONTINUE;
			Topic==?HTTPPAGE;
			Topic==?HTTPREDIRECT;
			Topic==?HTTPPAGEERROR;
			Topic==?HTTPSERVERERROR->
	?WEB;

t_parent(Topic)	when	Topic==?WEB;
			Topic==?NOWEB;
			Topic==?ERROR->
	?FROOT;

t_parent({?FACETMAP,"http1"++Rest})->
	?HTTPCONTINUE;

t_parent({?FACETMAP,"http2"++Rest})->
	?HTTPPAGE;

t_parent({?FACETMAP,"http3"++Rest})->
	?HTTPREDIRECT;

t_parent({?FACETMAP,"http4"++Rest})->
	?HTTPPAGEERROR;

t_parent({?FACETMAP,"http5"++Rest})->
	?HTTPSERVERERROR;

t_parent({?FACETMAP,"redirect"++Rest})->
	?HTTPREDIRECT;

t_parent(?HTTPERROR)->
	?WEB;

t_parent(Other)->
	?warn({no_parent,Other}),
	?NULL.
