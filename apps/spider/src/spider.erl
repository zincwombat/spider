-module(spider).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([filter/2,
	 getFilters/0]).

-export([
	 analyse/1,
	 getLocalLinks/2,
	 %%getMatch/2,
	 stripGetMatch/2,
	 getLinks/1,
	 i_analyseContent/2,
	 getRawContent/1,
	 getStrippedContent/1,
	 httpGet/1]).

-include("agent.hrl").
-include("spider.hrl").
-include("filter.hrl").

-define(NUMLOCAL,5).	%% number of local links to follow
-define(TAG,'$Name:  $').

-define(RAW_FILTER,	#filter_opt{	key=raw,
					type=?FILETYPE_ERL,
					desc="Web Spider Analysis"}).

-define(XML_FILTER,	#filter_opt{	key=xml,
					type=?FILETYPE_XML,
					desc="Web Spider Analysis"}).



analyse(URL)->
	%% this is the main entry point to the module

	Result=i_analyse({parse,url_parse(URL)},URL,#spider{root_domain=URL}),
	?info({analyse,{url,URL},{result,Result}}),
	Result.

i_analyse({parse,error},URL,S=#spider{})->
	analyseContent(i_mkurl(URL),S);

i_analyse({parse,{_Proto,_Host,_Port,_Path,_Params}},URL,S=#spider{})->
	analyseContent(URL,S);

i_analyse(Other,_URL,#spider{})->
	{error,Other}.

url_parse(URL)->
	%% verify that the URL is in the correct format
	%% http or https etc.
	case url_parse:parse(URL) of
	{Proto,{A,B,C,D},Port,Path,QS}->
		Host=lists:flatten(io_lib:format("~w.~w.~w.~w",[A,B,C,D])),
		{Proto,Host,Port,Path,QS};

	Other->
		Other
	end.


stripGetMatch(URL,Tag)->
	%% strip the tags from the html first, and then
	%% apply the regex matching

	F=fun(Z)->regex_controller:extractMatchPos(htmltag,Z,10000) end,
	N=(catch httpGet(mkurl(URL),F)),
	case N of
	{htmltag,B}->
		regex_controller:extractMatches(Tag,B); Other->
		Other
	end.

getMatch(URL,Tag)->
	%% apply the regex matching on the raw response

	F=fun(Z)->regex_controller:extractMatches(Tag,Z) end,
	(catch httpGet(mkurl(URL),F)).

%% return the raw content (including the HTML tags for the URL)
%% return is either <<binary>> or {error,Reason}

getRawContent(URL)->
	F=fun(Z)->Z end,
	(catch httpGet(mkurl(URL),F)).

%% return the content stripped of all HTML tags
%% return is either <<binary>> or {error,Reason}

getStrippedContent(URL)->
	F=fun(Z)->regex_controller:extractMatchPos(htmltag,Z,10000) end,
        (catch httpGet(mkurl(URL),F)).

stripHtml(Bin) when is_binary(Bin)->
	regex_controller:extractMatchPos(htmltag,Bin,10000).

%% Bin is the binary content to be searched for the regexp Tag

i_getMatch(Bin,Tag) when is_binary(Bin)->
	regex_controller:extractMatches(Tag,Bin);

i_getMatch(_,_)->
	[].

i_getMatch(Bin,Tag,N) when is_binary(Bin)->
	regex_controller:extractMatches(Tag,Bin,N);

i_getMatch(E={error,_Reason},_Tag,_)->
	E;

i_getMatch(Other,_Tag,_)->
	{error,Other}.

getLinks(URL)->
	getMatch(URL,href).

getLocalLinks(URL,Num)->
	getLocalLinks(URL,url_parse(URL),Num).

getLocalLinks(URL,U={_Proto,_Host,_Port,_Path,_},Num)->
	case getLinks(URL) of
	{href,Links}->
		%% first extract just the unique links
		%% add the root URL

		RootUrl=list_to_binary(i_mkurl(URL)),
		ULinks=lists:usort([RootUrl|Links]),

		%% then just the Local Links
		%% lets count the links as well

		Local=lists:filter(fun(Z)->isLocal(U,Z) end,ULinks),

		NumLocal=length(Local), %% num local links

		ALocal=take(Local,Num),

		Props=[{pages,NumLocal},{analysedPages,min(Num,NumLocal)}],

		{local_links,Props,lists:map(fun(Z)->ensureUrl(U,Z) end,ALocal)};

	Other->
		Other
	end;

getLocalLinks(URL,Other,_Num)->
	?warn({parse_failed,{url,URL},Other}),
	Other.

%% take maxiumn number of elements of list

take(L,Max) when is_list(L), is_integer(Max)->
	take(L,Max,[],0).

take([],_Max,Acc,_Num)->
	lists:reverse(Acc);

take(_L,Max,Acc,Max)->
	lists:reverse(Acc);

take([H|T],Max,Acc,Num)->
	take(T,Max,[H|Acc],Num+1).

ensureUrl(_U,Local= <<"http",_Rest/binary>>)->
	%% should make case independent http|HTTP -- TBD
	Local;

ensureUrl(_U={Proto,Host,default,_,_},Local= <<"/",_Rest/binary>>)->
	list_to_binary([atom_to_list(Proto),"://",Host,Local]);

ensureUrl(_U={Proto,Host,default,_,_},Local= <<_Rest/binary>>)->
	%% add "/" after the hostname
	list_to_binary([atom_to_list(Proto),"://",Host,"/",Local]);

ensureUrl(U={_Proto,_Host,_Port,_,_},Local)->
	{error,{ensureUrl,{unhandled,{U,Local}}}}.

processMetaDescriptions(S=[#spider_data{}|_])->

	%% check all of the META_DESCRIPTIONS for uniqueness
	%% used to qualify a domain for SEM application
	%% first gather all of the meta_description tuples

	MD=lists:map(fun(Z)->extract(meta_description,Z) end,S),

	%% eliminate duplicate empty lists

	MD1=lists:filter(fun(Z)->notEmpty(Z) end,MD),

	%% extract just the unique elements by loading the data into a set and counting
	%% the elements

	US=sets:to_list(sets:from_list(MD1)),

	[{meta_description_tags,length(MD1)},{unique_meta_description_tags,length(US)}];

processMetaDescriptions(Other)->
	?warn({processMetaDescriptions,{unhandled,Other}}),
	[].

processTitle(T={title,Title})->

        %% check all of the TITLE tages 
        %% used to qualify a domain for SEM application

	%% look for a pattern of tokens delimited by punctuation such as
	%% '|','-',',' etc

	{ok,RE}=re:compile("[|,-]"),
	Tks=
	case re:run(Title,RE) of
	{match,M}->
		length(M);
	Other->
		0
	end,
	[{title_keyword_sep,Tks}].

        

notEmpty([])->
	false;

notEmpty(_)->
	true.
	

extract(Tag,#spider_data{data=D})->
	lists:filter(fun(Z)->isFilter(Tag,Z) end,D);

extract(Tag,Other)->
	?warn({unexpected,{tag,Tag},{data,Other}}),
	[].

isFilter(Tag,{Tag,_})->
	true;

isFilter(_,_)->
	false.



analyseContent(URL,S=#spider{})->
	%% get all the links and spawn parallel processes to parse the 
	%% bodies for the contact details

	case getLocalLinks(URL,?NUMLOCAL) of
	{local_links,Props,Links}->

		% look for additional files useful as indicators for
		% SEM candidates

		R1=pathExists(URL,"robots.txt"),
		S1=pathExists(URL,"sitemap.xml"),

		RootProps=lists:append([R1,S1]),

		?info({getLocalLinks,Props,URL,Links}),

		%% generate a worker process for each Link in the
		%% Local Link set 

		Workers=lists:map(fun(Z)->spawn(?MODULE,i_analyseContent,[self(),Z]) end,Links),
		%% Workers contains the list of Worker Process Ids

		R=yield(Workers,[]),
		?dbug({yield,R}),

		SD=buildResponse(R),
		?dbug({sd,SD}),

		%% ensure that SD contains no empty elements

		SD1=lists:filter(fun(Z)->isSpiderData(Z) end,SD),
		?dbug({sd1,SD1}),

		CU=processMetaDescriptions(SD1),

		P=lists:append([CU,Props,RootProps]),

		Result=S#spider{props=P,data=SD1},
		?info({result,{url,URL},Result}),
		Result;

	Other->
		Other
	end.

isSpiderData(#spider_data{})->
	true;

isSpiderData(_)->
	false.

i_analyseContent(Pid,URL)->

	%% this function handles the content analysis for a single URL
	%% Pid is the parent process ID to which the results are to be
	%% sent.

	%% returns either a Binary or {error,Reason}

	?dbug({inside_i_analyseContent,{pid,Pid},{url,URL}}),

	case getRawContent(URL) of
	E={error,_Reason}->
		Pid ! {self(),{URL,E}};

	Bin when is_binary(Bin)->

		?info({binary,Bin}),
		{_,Stripped}=stripHtml(Bin),
		?info({stripped,Stripped}),

		

		Phone=i_getMatch(Stripped,phone),
		MetaKW=i_getMatch(Bin,meta_keyword,1),
		MetaDesc=i_getMatch(Bin,meta_description,1),
		Title=i_getMatch(Bin,title,1),
		Copyright=i_getMatch(Stripped,copyright),
		Email=i_getMatch(Bin,email),
		CompanyNumber=i_getMatch(Bin,company_nr),

		?info({{phone,Phone},{meta_keywords,MetaKW},{meta_desc,MetaDesc},{title,Title},{copy,Copyright}}),

		Pid ! {self(),{URL,[
				Title,
				Email,
				Phone,
				MetaKW,
				MetaDesc,
				CompanyNumber,
				Copyright]}};

	Other->
		Pid ! {self(),{URL,{error,Other}}}

	end.

pathExists({url,URL},Path)->
	%% add a path to a URL and check whether is exists
	case (catch httpGet({url,URL++"/"++Path})) of
	{error,Reason}->
		[{Path,false}];
	Bin->
		[{Path,true},{Path++"_size",erlang:size(Bin)}]
	end;
	

pathExists(URL,Path)->
	pathExists(mkurl(URL),Path).

%% take the raw analysis results and construct a response in the
%% required form

buildResponse(R) when is_list(R)->
	lists:map(fun(Z)->buildResponse(Z) end,R);

buildResponse(E={URL,{error,_Reason}})->
	?dbug({noBuild,E}),
	[];

buildResponse({URL,Analysis}) when is_binary(URL),is_list(Analysis)->
	buildResponse({binary_to_list(URL),Analysis});

buildResponse({URL,Analysis}) when is_list(Analysis)->
	%% create the response record and populate it with the
	%% analysis results

	Clean=isClean(URL),
	SSL=isSSL(URL),

	S=#spider_data{url=URL,props=[Clean,SSL]},
	B=lists:foldl(fun(Z,Acc)->buildResponse(Z,Acc) end,S,Analysis),
	?info({build,URL,B}),
	B;

buildResponse(Other)->
	?dbug({noBuild,Other}),
	[].

%% take each type of response data and load it into a spider_data{} record

buildResponse({Tag,[[Value]]},S=#spider_data{}) when is_binary(Value)->
	buildResponse({Tag,binary_to_list(Value)},S);

buildResponse({_Tag,[]},S=#spider_data{})->
	S;

buildResponse(T={title,Title},S=#spider_data{data=Data})->
	TX=text:wstrcompress(Title),
	KW=processTitle(T),
	R={title,KW,TX},
	S#spider_data{data=[R|Data]};

buildResponse({email,Emails},S=#spider_data{data=Data}) when is_list(Emails)->
	E=lists:map(fun(Z)->binary_to_list(Z) end,Emails),
	S#spider_data{data=[{email,lists:usort(E)}|Data]};

buildResponse({phone,Phones},S=#spider_data{data=Data}) when is_list(Phones)->
	P=lists:map(fun(Z)->binary_to_list(Z) end,Phones),
	S#spider_data{data=[{phone,lists:usort(P)}|Data]};

buildResponse({copyright,C},S=#spider_data{data=Data})->
	TX=text:wstrcompress(C),
	?info({textResult,copyright,TX}),
	S#spider_data{data=[{copyright,TX}|Data]};

buildResponse({meta_keyword,MK},S=#spider_data{data=Data})->
	TX=text:wstrcompress(MK),
	?info({textResult,meta_keyword,TX}),
	S#spider_data{data=[{meta_keyword,TX}|Data]};

buildResponse({company_nr,CN},S=#spider_data{data=Data})->
	TX=text:wstrcompress(CN),
	?info({textResult,company_nr,TX}),
	S#spider_data{data=[{company_nr,TX}|Data]};

buildResponse({meta_description,MD},S=#spider_data{data=Data})->
	TX=text:wstrcompress(MD),
	?info({textResult,meta_description,TX}),
	S#spider_data{data=[{meta_description,TX}|Data]};

buildResponse([],S=#spider_data{})->
	S;

buildResponse(Other,S=#spider_data{})->
	?warn({unhandledObject,Other}),
	S.

%% ensure that the scheme is present in the URL (https not yet supported)

mkurl(U={url,URL}) when is_list(URL)->
	U;

mkurl(URL)->
	{url,i_mkurl(URL)}.

i_mkurl(URL) when is_binary(URL)->
	i_mkurl(binary_to_list(URL));

i_mkurl(URL="http://"++_Rest)->
	URL;

i_mkurl(URL="https://"++_Rest)->
	URL;

i_mkurl(URL)->
	"http://"++URL.

%% check whether the URL contains any special characters

isClean({url,URL})->
	isClean(URL);

isClean(URL) when is_list(URL)->
	{ok,RE}=re:compile("[&?=]"),
	C=
	case re:run(URL,RE) of
	{match,[]}->
		true;
	Other->
		false
	end,
	{cleanurl,C}.

isSSL({url,URL})->
	isSSL(URL);

isSSL(URL) when is_list(URL)->
	{ok,RE}=re:compile("^https:"),
	C=
	case re:run(URL,RE) of
	{match,[]}->
		false;
	Other->
		true
	end,
	{ssl,C}.


%% HTTP Client facade ----------------------------

%% TBD, make a HEAD version of this ....


httpGet(URL)->
	httpGet(URL,[]).

httpGet({url,URL},F)->
	?dbug({httpGet,URL,F}),
	RequestHeaders=[{"User-Agent","erlang"},{"Accept","text/*"}],
	Response=httpc:request(get,{URL,RequestHeaders},[],[{body_format,binary}]),
	?dbug({response,Response}),
	httpResponseHandler(URL,Response,F);

httpGet(URL,_)->
	?dbug({error,{badurl,URL}}),
	{error,{badurl,URL}}.

%% END HTTP Client facade -----------------------

%% the default handler for the httpGet return ...

httpResponseHandler(_URL,{ok,Result},F)->
	?dbug({httpResponseHandler,{result,Result}}),
	case Result of
	E={error,_Reason}->
		?error({httpResponseHandler,E}),
		throw(E);

	{{_Proto,200,_Line},Headers,Body}->
		?dbug({httpResponseHandler,{{httpcode,200},{headers,Headers}}}),
		%% check that the content type is acceptable
		CT=lists:keysearch("content-type",1,Headers),
		case contentType(CT) of
		E={error,_Reason}->
			?dbug({unhandledContentType,E}),
			throw(E);
		ok->
			ok
		end,
		applyFun(Body,F);

	{{_Proto,Code,_Line},_Headers,_Body} when is_integer(Code)->
		throw({error,{httpCode,Code}});

	{StatusCode,Body}->
		?dbug({{statusCode,StatusCode},{body,Body}}),
		applyFun(Body,F);

	RequestId->
		throw({error,{unexpected,RequestId}})
	end;

httpResponseHandler(_,_Response=Other,_)->
	Other.

applyFun(Body,F) when is_function(F)->
	?dbug({applying_fun,F}),
	Result=F(Body),
	?dbug({fun_result,Result}),
	Result;

applyFun(Body,_)->
	Body.

%% return true/false based on content-type header

contentType({value,{_,"text/html"++_Rest}})->
	ok;

contentType({value,{_,"text/plain"++_Rest}})->
	%% e.g. for robots.txt
	ok;

contentType({value,{_,"text/xml"++_Rest}})->
	%% e.g. for sitemap.xml
	ok;

contentType(Unhandled)->
	?error({badContentType,Unhandled}),
	{error,{badContentType,Unhandled}}.


yield([],Results)->
	?dbug({alldone,Results}),
	Results;

yield(Pids,Results)->
	receive
		X={Pid,Result}->
			NewPids=lists:delete(Pid,Pids),
			?dbug({{received,X},{waiting,NewPids}}),
			yield(NewPids,lists:append([Result],Results));

		Other->
			?warn({yield,{unexpectedMessage,Other}}),
			yield(Pids,Results)

		after 60000->
			?warn({timeout,60000}),
			lists:map(fun(Z)->exit(Z,stop) end,Pids),
			yield([],Results)
	end.
	
%% TBD - fixup below to handle https ....

isLocal({_Proto,Host,_Port,_Path,_Params},<<"http://",Rest/binary>>)->
	i_isLocal(Host,binary_to_list(Rest));

isLocal(_URLP,_HRef)->
	true.

i_isLocal(URL,NewURL)->
	%% check if NewURL contains the URL
	%%RE=URL++"/",
	%%RE=URL,
	{ok,RE}=re:compile(URL),
	?info({regexp,RE,newurl,NewURL}),
	case re:run(NewURL,RE) of
	{match,[{_Start,_Len}|_]}->
		?info({isLocal,URL,true}),
		true;
	Other->
		?info({isLocal,URL,false,Other}),
		false
	end.

%% XML filters -----------------------------------------



getFilters()->
	[?XML_FILTER,?RAW_FILTER].

filter(xml,startJob)->
	{filter,xmlmisc:startXml(root,[])};

filter(xml,CC=#controllerConf{})->
	{filter,serialise(CC)};

filter(xml,JC=#jobConf{})->
	{filter,serialise(JC)};

filter(xml,endJob)->
	{filter,xmlmisc:endXml(results)++xmlmisc:endXml(root)};

filter(xml,A=#agentResult{	
		payload=E={error,Reason}})->
	{filter,null};

filter(xml,A=#agentResult{	
		payload=E={'EXIT',Reason}})->
	{filter,null};

filter(xml,A=#agentResult{})->
	case (catch serialise(A)) of
	E={error,Reason}->
		?warn({serialiseFailed,A,E}),
		{filter,null};

	EE={'EXIT',Reason}->
		?warn({serialiseFailed,A,EE}),
		{filter,null};

	Other->
		?dbug({serialiseOk,A,Other}),
		{filter,Other}
	end;

%% end XML filters -------------------------------------

filter(raw,#agentResult{payload=P})->
	filter(raw,P);

filter(raw,S=#spider{})->
	X=io_lib:format("~p",[S]),
	{filter,X};

filter(Type,Payload)->
	?dbug({noFilter,{Type,Payload}}),
	{filter,null}.

%% XML serialisers -------------------------

serialise(CC=#controllerConf{
		version=Version,
		numagents=NumAgents,
		res_ns=Res_NS})->

	xmlmisc:mk_element(control,[{version,Version},{numagents,NumAgents}],[])
	++ xmlmisc:startXml(results,[]);

serialise(JC=#jobConf{
		version=Version,
		jobid=JobId,
		moduleid=ModuleId,
		jobdesc=JobDesc,
		importSpec=#import{type=Type,format=Format,value=Value},
		logfile=LogFile,
		logdir=LogDir})->

	xmlmisc:mk_element(job,[{tag,?TAG},{version,Version},{moduleid,ModuleId},{sourcetype,Type},{format,Format},{value,Value}],[]);


serialise(A=#agentResult{	
		version=Version,
		agentID=AgentID,
		timestamp=Timestamp,
		payload=E={error,Reason}})->
	throw(E);

serialise(A=#agentResult{	
		version=Version,
		agentID=AgentID,
		timestamp=Timestamp,
		payload=PayLoad})->

        TT=calendar:now_to_local_time(Timestamp),
	V=xmlmisc:formatUTC(TT),

	xmlmisc:startXml(agent,[{timestamp,V},{id,AgentID},{urn,Version}])++
	serialise(PayLoad)++
	xmlmisc:endXml(agent);

serialise(P=#spider{
		version=Version,
		root_domain=Root,
		props=Props,
		data=Data })->

	?dbug({serialising,P}),

	xmlmisc:mk_element(domain,[],Root)++
	xmlmisc:startXml(spider,Props,[])++
	serialise(Data)++
	xmlmisc:endXml(spider);

serialise(SD=[S=#spider_data{}|Rest])->
	lists:map(fun(Z)->serialise(Z) end,SD);

serialise(S=#spider_data{version=Version,
			 url=Url,
			 props=Props,
			 data=Data})->

	Attrs=lists:append([[{version,Version},{url,Url}],Props]),

	xmlmisc:startXml(spider_data,Attrs)++
	serialise(Data)++
	xmlmisc:endXml(spider_data);

serialise(E={error,Reason})->
	throw(E);

serialise(SD) when is_list(SD)->
	lists:map(fun(Z)->serialise(Z) end,SD);

serialise({Tag,Attrs,Ls=[H|_]}) when is_list(Attrs),is_integer(H)->
	xmlmisc:mk_element(Tag,Attrs,Ls);

serialise({Tag,Ls=[H|_]}) when is_integer(H)->
	xmlmisc:mk_element(Tag,[],Ls);

serialise({Tag,Ls}) when is_list(Ls)->
	lists:map(fun(Z)->xmlmisc:mk_element(Tag,[],Z) end,Ls);

serialise(Other)->
	E={noSerialiser,Other},
	?warn(E),
	throw(E).

	



min(A,B) when is_integer(A),is_integer(B),A<B->
	A;

min(A,B)->
	B.
