-module(xmlmisc).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([addXMLHeader/1,
	mk_element/2,
	mk_element_ns/4,
	mk_element/3,
	mk_element/4,
	addAttribute/2,
	getAttribute/2,
	formatUTC/0,
	xmlEntityEncode/1,
	formatUTC/1,
	content/1,
	xmlpi/0,
	xmlxsl/1,
	nsinfo/1]).

-export([getNameSpacePart/1,
	 extractNSUrl/2]).

-export([startXml/2,
	 startXml/3,
	 endXml/1,
	 endXml/2,
	 nsprefix/2]).

-include("xml.hrl").
-include("xmerl.hrl").

xmlpi()->
	?XML_HEADER.

xmlxsl(Ref)->
	X=io_lib:format("<?xml-stylesheet type='text/xsl' href='~s'?>",[Ref]),
	lists:flatten(X).

addXMLHeader(Body)->
	io_lib:format("~s~n~s",[?XML_HEADER,Body]).


nsprefix(Element,[])->
	Element;

nsprefix(Element,NSInfo) when record(NSInfo,nsinfo)->
	nsprefix(Element,NSInfo#nsinfo.prefix);

nsprefix(Element,NSPrefix) when atom(NSPrefix)->
	nsprefix(Element,atom_to_list(NSPrefix));

nsprefix(Element,NSPrefix)->
	io_lib:format("~s:~s",[NSPrefix,Element]).

startXml(ElementName,[])->
	io_lib:format("<~s>",[ElementName]);

startXml(ElementName,AttList)->
	RevAttList=lists:reverse(AttList), 	%% added 21/10
	io_lib:format("<~s~s>",[ElementName,serialise(RevAttList,[])]).

startXml(ElementName,AttList,[])->
	startXml(ElementName,AttList);

startXml(ElementName,AttList,Prefix)->
	E=io_lib:format("~s:~s",[Prefix,ElementName]),
	startXml(E,AttList).

endXml(ElementName)->
	io_lib:format("</~s>",[ElementName]).

endXml(ElementName,[])->
	endXml(ElementName);

endXml(ElementName,Prefix)->
	io_lib:format("</~s:~s>",[Prefix,ElementName]).

serialise([],Acc)->
	Acc;

serialise([[]|T],Acc)->
	serialise(T,Acc);

serialise([{Name,Value}|T],[])->
	io_lib:format(" ~s",[serialise(T,mk_attribute(Name,Value))]);

serialise([{Name,Value}|T],Acc)->
	io_lib:format("~s ~s",[serialise(T,mk_attribute(Name,Value)),Acc]);

serialise(Other,Acc)->
	exit({serialise,Other}).


getAttribute(Attribute,AttributeList)->
        case lists:keysearch(Attribute,1,AttributeList) of
        {value,{Attribute,Value}}->
                Value;
        _->
                []
        end.

addAttribute(AttList,{Name,Value}) when atom(Value)->
	addAttribute(AttList,{Name,atom_to_list(Value)});

addAttribute(AttList,{Name,Value})->
	lists:append([{Name,Value}],AttList).

xmlString(Value) when is_integer(Value)->
	xmlString(integer_to_list(Value));

xmlString(Value)->
	V=lists:flatten(xmlEntityEncode(Value)),
	io_lib:format("'~s'",[V]).

xmlns([])->
	"xmlns";

xmlns(Prefix)->
	io_lib:format("xmlns:~s",[Prefix]).

mk_attribute(Name,Value)->
	io_lib:format("~s=~s",[Name,xmlString(Value)]).

mk_element(ElementName,Body)->
	mk_element(ElementName,[],Body).

%%=============================================================================
%% BEGIN Main Serialiser 
%%=============================================================================

mk_element(ElementName,AttList,Body) when is_atom(ElementName),
					  is_list(AttList)->
	mk_element(atom_to_list(ElementName),AttList,Body);

mk_element(ElementName,AttList,Body={Tag,Payload})->
	X=
	case is_list(Payload) of
	true->
		case io_lib:printable_list(Payload) of
		true->
			mk_element(Tag,[],Payload);
		false->
			lists:map(fun(Z)->mk_element(Tag,[],Z) end,Payload)
		end;
	false->
		?critical({cannot_serialise,Payload}),
		[]
	end,
	xmlmisc:mk_element(ElementName,AttList,X);

mk_element(ElementName,AttList,Body=[H|_T]) when is_tuple(H)->
	lists:map(fun(Z)->mk_element(ElementName,AttList,Z) end,Body);

mk_element(ElementName,AttList,Body)->
	io_lib:format("~s~s~s",[lists:flatten(startXml(ElementName,AttList)),
				xmlEntityEncode(Body),
				lists:flatten(endXml(ElementName))]).

%%=============================================================================
%% END Main Serialiser 
%%=============================================================================

mk_element(ElementName,AttList,Body,NSInfo) when record(NSInfo,nsinfo)->
	mk_element(nsprefix(ElementName,NSInfo#nsinfo.prefix),AttList,Body);

mk_element(ElementName,AttList,Body,{NSPrefix,_})->
	mk_element(nsprefix(ElementName,NSPrefix),AttList,Body);

mk_element(ElementName,AttList,Body,NSPrefix)->
	mk_element(nsprefix(ElementName,NSPrefix),AttList,Body).

mk_element_ns(ElementName,AttList,Body,NSInfo) when record(NSInfo,nsinfo)->
	NSPrefix=NSInfo#nsinfo.prefix,
	NSUrl=NSInfo#nsinfo.uri,
        mk_element_ns(ElementName,AttList,Body,{NSPrefix,NSUrl});

mk_element_ns(ElementName,AttList,Body,{NSPrefix,NSURL})->
        mk_element(nsprefix(ElementName,NSPrefix),addAttribute(AttList,{xmlns(NSPrefix),NSURL}),Body).
		

content({XML,Rest}) when record(XML,xmlElement)->
        content(XML);

%% We extract the nsinfo as well ....
content(XML) when record(XML,xmlElement)->
        %% note that we "flatten" the content to remove empty text nodes
        %% caused by \n, \t, etc in the source XML files

        {XML#xmlElement.name,
	 getNsInfo(XML),	%% THIS WILL BREAK MUCH !!!! (repl above)
         content(XML#xmlElement.attributes),
         lists:flatten(content(XML#xmlElement.content))};

content(XML) when list(XML)->
        lists:map(fun(Z)->content(Z) end,XML);

content(XML) when record(XML,xmlText)->
        %% we need to eliminate empty text nodes somehow
        strip(XML#xmlText.value);

content(XML) when record(XML,xmlAttribute)->
        {XML#xmlAttribute.name,XML#xmlAttribute.value};

content(XML)->
        %%{unknown,XML}.
        throw({error,{xml_parse,{unknown,XML}}}).

getNsInfo(XML) when record(XML,xmlElement)->
	getNsInfo(XML#xmlElement.expanded_name,
		  XML#xmlElement.nsinfo,
		  XML#xmlElement.namespace).

getNsInfo(EName,[],XMLNameSpace)->
	#nsinfo{element=toList(EName),
		prefix=[],
		uri=toList(XMLNameSpace#xmlNamespace.default)};

getNsInfo(EName,{Prefix,Name},XMLNameSpace)->
	URI=
	case lists:keysearch(Prefix,1,XMLNameSpace#xmlNamespace.nodes) of
	{value,{Prefix,U}}->
		U;
	false->
		throw(?error({getNsInfo,keysearch_fail,Prefix}))
	end,
	#nsinfo{element=toList(Name),
		prefix=toList(Prefix),
		uri=toList(URI)}.


toList(L) when atom(L)->
	atom_to_list(L);

toList(L)->
	L.

strip([$\n |T])->strip(T);
strip([$\t |T])->strip(T);
strip([$\r |T])->strip(T);
strip([$ |T])->strip(T);
strip(Rest)->Rest.

nsinfo({Tag,Attrs,Elements})->
	{Prefix,E}=getNameSpacePart(Tag),
	{_,URI}=extractNSUrl(Prefix,Attrs),
	#nsinfo{element=E,
		prefix=Prefix,
		uri=URI}.

getNameSpacePart(Name) when atom(Name)->
	getNameSpacePart(atom_to_list(Name));

getNameSpacePart(Name) when list(Name), is_integer(hd(Name))->
	case regexp:split(Name,":") of
	{ok,[NSPrefix,Element]}->
		{NSPrefix,Element};

	{ok,[Element]}->
		{null,Element};

	{ok,Other}->
		{error,nsprefix};

	Other->
		Other
	end.


extractNSUrl({NSPrefix,Element},Attributes) when list(NSPrefix)->
	extractNSUrl(NSPrefix,Attributes);

%%extractNSUrl(null,Attributes)->
extractNSUrl({null,Element},Attributes)->
	Token=list_to_atom("xmlns"),
	case lists:keysearch(Token,1,Attributes) of
	{value,{Token,NSURL}}->
		{null,NSURL};

	false->
		{null,no_namespace}
	end;

extractNSUrl(NSPrefix,Attributes) when list(NSPrefix)->
	Token=list_to_atom("xmlns:"++NSPrefix),
	case lists:keysearch(Token,1,Attributes) of
	{value,{Token,NSURL}}->
		{list_to_atom(NSPrefix),NSURL};

	false->
		{null,no_namespace}
	end.
	


%% convert a time in {{YY,MM,DD},{HH,M,SS}} format to XML datetime format
                                                                                
y(Y1, Y2) -> 256 * Y1 + Y2.
                                                                                
formatUTC()->
        %T=yaws:date_and_time(),
        %i_formatUTC(T).
	formatUTC(calendar:local_time()).

formatUTC(Z={_,_,_})->
	%% erlang:now format
	UTC=calendar:now_to_local_time(Z),
	formatUTC(UTC);
                                                                                
formatUTC(Z={{_,_,_},{_,_,_}})->
 %       Universal=calendar:local_time_to_universal_time(Z),
 %       T=yaws:date_and_time(Z,Universal),
         i_formatUTC(Z);
                                                                                
formatUTC(Z)->
        Z.
                                                                                
%i_formatUTC([Y1,Y2, Mo, D, H, M, S | Diff])->
i_formatUTC({{Y,Mo,D},{H,M,S}})->
        P=io_lib:format("~w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0w",
                        [Y,Mo,D,H,M,S]),
        lists:flatten(P).
	

xmlEntityEncode([Ls])->
	xmlEntityEncode(Ls);

xmlEntityEncode(Ls) when is_list(Ls)->
        Z=lists:map(fun(Z)->map(Z) end,Ls);

xmlEntityEncode(Ls) when is_atom(Ls)->
        Z=lists:map(fun(Z)->map(Z) end,atom_to_list(Ls));

xmlEntityEncode(Ls) when is_integer(Ls)->
        Z=integer_to_list(Ls);

xmlEntityEncode(Other)->
	?warn({badarg,Other}),
        throw({error,badarg}).

map($<)->
        "&lt;";

map($>)->
        "&gt;";

map($&)->
        "&amp;";

map($\")->
        "&quot;";

map(Other)->
        Other.



ensure_atom(Atom) when is_atom(Atom) ->
    Atom;
ensure_atom(String) when is_list(String), is_integer(hd(String)) ->
    list_to_atom(String);
ensure_atom(List) when is_list(List) ->
    [ensure_atom(Item) || Item <- List];
ensure_atom(Other) ->
    Other.
