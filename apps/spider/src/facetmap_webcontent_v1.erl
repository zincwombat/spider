-module(facetmap_webcontent_v1).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").
-export([classify/2,
	 filter/2,
	 getLocalLinks/1,
	 getContacts/1,
	 i_spider/2,
	 httpGet/1,
	 test/1,
	 spider/1,
	 spider/2]).

-include("agent.hrl").
-include("facetmap_webcontent_v1.hrl").

-define(REGEXP_H1,		{?HEADER1,"<H1\s*"}).
-define(REGEXP_TITLE,		{?TITLE,"<TITLE\s*"}).
-define(REGEXP_METADESCRIPTION,	{?METADESCRIPTION,"<META NAME=[\"\']DESCRIPTION[\"\']\s*"}).
-define(REGEXP_METAKEYWORD,	{?METAKEYWORD,"<META NAME=[\"\']KEYWORDS[\"\']\s*"}).
-define(REGEXP_IMG,		{?IMAGE,"<IMG\s+[^>]*>"}).
-define(REGEXP_ALT,		"ALT\s*=").
-define(RMAILTO,		"MAILTO:[0-9A-Z_]+\@[0-9A-Z_.-]+").
-define(REGEXP_EMAIL,   	{?EMAIL,?RMAILTO}).
-define(REGEXP_LINK,    	{?LINK,"HREF\s*=\s*HTTP[S]*:[0-9A-Z_/.]+"}).
-define(REGEXP_POST,    	{?POST,"METHOD\s*=\s*POST"}).
-define(REGEXP_FLASH,    	{?FLASH,"\<OBJECT\s+.+MACROMEDIA"}).
-define(CONTENT_SIZE,    	{?SIZE,null}).
-define(RLINK,			"(HREF\s*=\s*[\"\']?HTTP[S]?:[0-9A-Z_/.]+)|(HREF\s*=\s*[\"\']?[0-9A-Z_/.]+)").
-define(RACN,			"ACN").
-define(COPYRIGHT,		"(COPYRIGHT\s+.+$)|(&copy;)").
-define(PHONE,			"PH[:]*\s+.+$").
-define(RURL,			"HTTP[S]?:[0-9A-Z_/.]+").
-define(HREF,			"HREF\s*=\s*[\"\']?HTTP[S]?:[0-9A-Z_/.]+").



test(URL)->
	i_test(mkurl(URL)).

i_test(URL)->
	F=fun(Z)->i_getLinks(URL,Z) end,
	httpGet(URL,F).

%% ensure that the scheme is present in the URL (https not yet supported)

mkurl(URL="http://"++Rest)->
	URL;

mkurl(URL="HTTP://"++Rest)->
	"http://"++Rest;

mkurl(URL)->
	"http://"++URL.

%% HTTP Client facade ----------------------------


httpGet(URL)->
	httpGet(URL,[]).

httpGet(URL="http://"++Rest,F)->
	?dbug({httpGet,URL,F}),
	Response=http:request(URL),
	httpResponseHandler(URL,Response,F);

httpGet(URL,_)->
	?dbug({error,{badurl,URL}}),
	{error,{badurl,URL}}.

%% END HTTP Client facade -----------------------

%% the default handler for the httpGet return ...

httpResponseHandler(URL,Response={ok,Result},F)->
	?dbug({httpResponseHandler,Response,F}),
	case Result of
	{StatusLine,Headers,Body}->
		?dbug({{status,StatusLine},{headers,Headers},{body,Body}}),
		applyFun(Body,F);

	{StatusCode,Body}->
		?dbug({{statusCode,StatusCode},{body,Body}}),
		applyFun(Body,F);

	RequestId->
		throw({error,{unexpected,RequestId}})
	end;

httpResponseHandler(_,Response=Other,_)->
	Other.

applyFun(Body,F) when is_function(F)->
	F(Body);

applyFun(Body,_)->
	Body.

%% ----------------------------------------------


getContacts(URL)->
	LocalLinks=getLocalLinks(URL),
	?dbug({{url,URL},{local,LocalLinks}}),
	%%Results=lists:map(fun(Z)->i_spider(self(),Z) end,LocalLinks).
	Workers=lists:map(fun(Z)->spawn(?MODULE,i_spider,[self(),Z]) end,LocalLinks),
	yield(self(),Workers,[]).

yield(Self,[],Results)->
	?dbug({alldone,Results}),
	Results;

yield(Self,Pids,Results)->
	receive
		X={Pid,Result}->
			NewPids=lists:delete(Pid,Pids),
			?dbug({{received,X},{waiting,NewPids}}),
			yield(Self,NewPids,lists:append([Result],Results));

		Other->
			?dbug({yield,{unexpectedMessage,Other}})
	end.
	

i_getLinks(URL,Body)->
	R=regexp:matches(canon(Body),?RURL),
	extractRegexpMatch(URL,Body,R).
		

getLocalLinks(URL)->
	%% download the body and parse the content for links
	R=ihttpc:hget(URL),
	httpGet(URL),
	?dbug({getLocalLinks,URL,R}),
	RR=getLocalLinks(URL,R).

getLocalLinks(URL,{ok,{{_Proto,200,_},_Headers,Body}})->
	C=canon(Body),
	Z=localLinks(URL,C,Body),
	lists:append([["http://"++URL],Z]);

getLocalLinks(URL,Other)->
	?warn({cannotSpider,URL,Other}),
	[].


spider(URL)->
	?dbug({spider,URL}),
	R=ihttpc:hget(URL),
	spider(URL,R).

spider(URL,{ok,{{_Proto,200,_},_Headers,Body}})->
                C=canon(Body),
                ZZ=localLinks(URL,C,Body),
		URLs=lists:append([["http://"++URL],ZZ]),
		?dbug({local,URLs}),
		lists:map(fun(Z)->i_spider(self(),Z) end,URLs);

spider(URL,Other)->
		?warn({cannotSpider,URL,Other}),
		[].

i_spider(Pid,URL)->
		?dbug({spidering,URL}),
		R=ihttpc:hget(URL),
		i_spider(Pid,URL,R).

i_spider(Pid,URL,{ok,{{_Proto,200,_},_Headers,Body}})->
		Contacts=extractContacts(URL,Body,canon(Body)),
		BusNos=extractBusNos(URL,Body,canon(Body)),
		Phone=extractPhone(URL,Body,canon(Body)),
		Pid ! {self(),{URL,Contacts,BusNos,Phone}};

i_spider(Pid,URL,Other)->
                ?warn({cannotSpider,URL}),
		Pid ! {self(),{URL,failed}}.


classify(URL,{ok,{{_Proto,200,_},_Headers,Body}})->
		?dbug({body,Body}),
		C=canon(Body),
		classify_body(URL,C);

classify(URL,{ok,{{_Proto,Code,_},_Headers,_Body}})->
		?dbug({contentError,URL,Code}),
		?MKTOPIC(Code);

classify(_URL,{ok,Code}) when is_integer(Code)->
		?MKTOPIC(Code);

classify(_URL,{error,{dnserr,DNSError}}) when is_atom(DNSError)->
		?DNSERROR;

classify(_URL,{error,no_a})->
		?NOARECS;

classify(_URL,{error,econnrefused})->
		?ECONNREFUSED;

classify(_URL,{error,{tcp_connect,{timeout,_Timeout}}})->
		?TCPERROR;

classify(_URL,{error,{tcp_connect,_Reason}})->
		?TCPERROR;

classify(_URL,{error,{http,_Reason}})->
		?HTTPERROR;

classify(_URL,{error,{http_timeout,_Timeout}})->
		?HTTPTIMEOUT;

classify(_URL,{error,{badarg,_Reason}})->
		?BADARG;

classify(URL,Other)->
		?warn({noClassification,{url,URL},Other}),
		?BADARG.

classify_body(URL,Body)->
	%% check for:
	%% 1) missing <TITLE> tag
	%% 2) missing <DESCRIPTION> tag
	%% 3) missing <H1> tag
	%% 4) missing meta keywords tag
	%% 5) check for IMG with no ALT tags


	RE1=match(?REGEXP_H1,Body),
	RE2=match(?REGEXP_TITLE,Body),
	RE3=match(?REGEXP_METADESCRIPTION,Body),
	RE4=match(?REGEXP_METAKEYWORD,Body),
	RE5=match(?REGEXP_IMG,Body),
	RE7=match(?REGEXP_EMAIL,Body),
	%%RE8=match(?REGEXP_LINK,Body),
	RE9=match(?CONTENT_SIZE,Body),
	RE10=match(?REGEXP_POST,Body),
	RE11=match(?REGEXP_FLASH,Body),
	{_,RE}=?REGEXP_IMG,
	RE6=checkImages(regexp:matches(Body,RE),Body),
	XX=getLinks(URL,Body),
	Result=[RE1,RE2,RE3,RE4,RE5,{?IMAGENOALT,RE6},RE7,RE9,RE10,RE11,XX],
	?dbug({result,Result}),
	{URL,Result}.

extractContacts(URL,OrigBody,Body)->
	R=regexp:matches(Body,?RMAILTO),
	RR=extractRegexpMatch(URL,OrigBody,R).

extractBusNos(URL,OrigBody,Body)->
	%%R=regexp:matches(Body,?COPYRIGHT),
	R=regexp:matches(Body,?RURL),
	RR=extractRegexpMatch(URL,OrigBody,R).

extractPhone(URL,OrigBody,Body)->
	R=regexp:matches(Body,?PHONE),
	RR=extractRegexpMatch(URL,OrigBody,R).

localLinks(URL,Body,OrigBody)->
	R1=regexp:matches(Body,?HREF),
%%	R1=regexp:matches(Body,?RLINK),
%%	R1=regexp:matches(Body,?RURL),
	RR1=extractRegexpMatch(URL,OrigBody,R1),
	LL=ilocalLinks(URL,{links,RR1}),
	lists:map(fun(Z)->url(URL,Z) end,LL).

ilocalLinks(URL,{links,Links}) when is_list(Links)->
	ULinks=sets:from_list(Links),
	S=sets:to_list(ULinks),
	L=lists:map(fun(Z)->iurl(Z) end,S),
	LocalLinks=lists:filter(fun(Z)->isLocal(URL,Z) end,L),
	LocalLinks.


isLocal(URL,"/"++_Rest)->
	true;

isLocal(URL,"http://"++NewURL)->
	i_isLocal(URL,NewURL);

isLocal(URL,"HTTP://"++NewURL)->
	i_isLocal(URL,NewURL);

isLocal(URL,_)->
	false.

i_isLocal(URL,NewURL)->
	%% check if NewURL contains the URL
	RE=URL++"/",
	case regexp:match(NewURL,RE) of
	{match,Start,Len}->
		true;
	Other->
		false
	end.


url(_,URL="HTTP://"++Rest)->
	"http://"++Rest;

url(_,URL="http://"++_Rest)->
	URL;

url(URL,Path)->
	"http://"++URL++Path.

iurl("HREF=\""++Rest)->
	Rest;

iurl("href=\""++Rest)->
	Rest;

iurl(_)->
	[].

getLinks(URL,Body)->
	%%R=regexp:matches(Body,"HTTP:[0-9A-Z_/.]+"),
	R1=regexp:matches(Body,"(HREF\s*=\s*[\"\']?HTTP[S]?:[0-9A-Z_/.]+)|(HREF\s*=\s*[\"\']?[0-9A-Z_/.]+)"),

	RR1=extractRegexpMatch(URL,Body,R1),
	?dbug({links,RR1}),
	categoriseLinks(URL,RR1).

extractRegexpMatch(_URL,Body,{match,Matches})->
	printMatch(Body,Matches);

extractRegexpMatch(_URL,_Body,Other)->
	?warn({regexpError,Other}),
	[].

countLinkTypes(List) when is_list(List)->
	{Int,Ext}=lists:foldl(fun(Z,Acc)->countLinkTypes(Z,Acc) end,{0,0},List),	{{internalPages,Int},{externalRefs,Ext}}.

countLinkTypes({_Link,local,_Count},{Internal,External})->
	{Internal+1,External};

countLinkTypes({_Link,external,_Count},{Internal,External})->
	{Internal,External+1}.

categoriseLinks(URL,Links) when is_list(Links)->
	?dbug({links,Links}),
%%	compute links and counts
	LTab=ets:new(ltab,[set,private]),
	lists:map(fun(Z)->categoriseLinks(LTab,URL,{link,Z}) end,Links),
	countLinkTypes(ets:tab2list(LTab)).

categoriseLinks(LTab,URL,{link,Link})->
	%% lets find out whether the link is a local or
	%% an external link

	%% canonify the URL to uppercase and remove the http:// prefix
	%% if one exists....

	R1=
	case canon(URL) of
	"HTTP://"++Rest-> 
		Rest;
	Other->
		Other
	end,


	?dbug({regexp,{Link,R1}}),
	LinkType=
	case regexp:matches(Link,R1) of
	{match,[]}->
		case regexp:matches(Link,"HTTP://") of
		{match,[]}->
			local;
		_->
			external
		end;
	_->
		local
	end,

	?dbug({classification,R1,Link,LinkType}),

	case ets:lookup(LTab,Link) of
	[]->
		ets:insert(LTab,{Link,LinkType,1});
	[{_,_,Count}]->
		ets:insert(LTab,{Link,LinkType,Count+1})
	end.

canon(String) when is_list(String)->
	%% canonicalise the string, initially just convert all
	%% lcase to ucase. This makes the regexp searches simple
	lists:map(fun(Z)->uc(Z) end,String).

uc(Z) when is_integer(Z),Z=<$z,Z>=$a->
	Z-32;

uc(Z)->
	Z.

nomatch({match,[]})->
	true;

nomatch(_)->
	false.

checkImages({match,PList},Body)->
	%% TBD this needs function clause error handling in case a bad regexp
	%% is used.

	XX=lists:map(fun({A,B})->string:substr(Body,A,B) end,PList),
	YY=lists:map(fun(Z)->checkImgNoAlt(Z) end,XX),
	%% now we need to count the {match,[]} elements in YY
	ZZ=lists:filter(fun(Z)->nomatch(Z) end,YY),
	length(ZZ).

checkImgNoAlt(String)->
	%% we need to check each string for presence of SRC= but
	%% not ALT= ....
	regexp:matches(String,"ALT\s*=").

match(?CONTENT_SIZE,Body)->
	?dbug({sizeArg,Body}),
	{?SIZE,length(Body)};

match({Tag,Regexp},Body)->
	?dbug({{regexp,Regexp},{body,Body}}),
	R=regexp:matches(Body,Regexp),
	Count=
	case R of
	{match,[]}->
		0;

	{match,Matches}->
		length(Matches);

	_->
		0
	end,
		
	{Tag,Count}.

printMatch(Content,Matches) when is_list(Matches)->
	lists:map(fun(Z)->printMatch(Content,Z) end,Matches);

printMatch(Content,{Start,Len})->
	string:substr(Content,Start,Len).

	

%% Topic analysis

f([T1,_T2,?WEB])->
	[T1,?WEB];

f(Ts)->
	Ts.

t_parent(Topic) when	Topic==?BADARG;
			Topic==?OTHERERROR->
	?ERROR;

t_parent(Topic)	when	Topic==?DNSERROR;
			Topic==?NOARECS;
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

t_parent({?FACETMAP,"http1"++_Rest})->
	?HTTPCONTINUE;

t_parent({?FACETMAP,"http2"++_Rest})->
	?HTTPPAGE;

t_parent({?FACETMAP,"http4"++_Rest})->
	?HTTPPAGEERROR;

t_parent({?FACETMAP,"http5"++_Rest})->
	?HTTPSERVERERROR;

t_parent({?FACETMAP,"redirect"++_Rest})->
	?HTTPREDIRECT;

t_parent(?HTTPERROR)->
	?WEB;

t_parent(Other)->
	?warn({no_parent,Other}),
	?NULL.


filter(Type,#agentResult{payload=PayLoad})->
        filter(Type,PayLoad);

filter(content,{webStats,{Domain,List}})->
        Result={Domain,filter(content,List)},
        {filter,io_lib:format("~p~n",[Result])};

filter(content,{error,Reason})->
        {filter,null};

filter(content,List) when is_list(List)->
        lists:map(fun(Z)->filter(content,Z) end,List);

filter(content,{{"urn:facetmap:webcontent_v1",Tag},Count})->
        {Tag,Count};

filter(_Content,Rest)->
        Rest.

