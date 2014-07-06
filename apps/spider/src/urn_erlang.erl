-module(urn_erlang).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([serialise/1,
	 mk_error/1,
	 getNS/0,
	 getSchema/0,
	 getXSL/0,
	 getXSL/1]).

-trace_level(warn).

-include("xmlinfo.hrl").

-define(THIS_XMLNS,	xmlinfo=?XML(?SYSTEMNS)).

%%=============================================================================
%% Root Element Serialiser
%%=============================================================================

serialise(Error=#error{?THIS_XMLNS,description=Description})->
	?dbug({serialiseRequest,Error}),
	Attribs=getAttributes(Error),
	XML=
	xmlmisc:startXml(error,Attribs,?PREFIX)++
	serialise(?PREFIX,Description)++
	xmlmisc:endXml(error,?PREFIX),
	lists:flatten(XML).

getAttributes(#error{?THIS_XMLNS,description={dns,DNSError}})->
	[{class,dns}];

getAttributes(#error{?THIS_XMLNS,description=Description})->
	[].

%%=============================================================================
%% Content Serialisers
%%=============================================================================

serialise(Pfx,E={Class,{Tag,Reason}}) when is_atom(Tag),is_atom(Class)->
	ClassStr=atom_to_list(Class),
	TagStr=atom_to_list(Tag),
	case utils:is_string(Reason) of
	true->
		ErrorString=io_lib:format("~s(~s):~s",[ClassStr,TagStr,Reason]),
		xmlmisc:mk_element(description,[],ErrorString,Pfx);
	_->
		?warn({noSerialiser,E}),
                []
	end;

serialise(Pfx,E={dns,DNSError}) when is_atom(DNSError)->
	ErrorString=io_lib:format("~s",[atom_to_list(DNSError)]),
	xmlmisc:mk_element(description,[],ErrorString,Pfx);

serialise(Pfx,Description)->
	case utils:is_string(Description) of
	true->
        	serialise(text,Pfx,Description);
	_->
		?warn({noSerialiser,Description}),
		[]
	end.

%==============================================================================
% Simple content type serialisers
%==============================================================================

serialise(_Type,Pfx,{Tag,#xmlinfo{},undefined})->
	xmlmisc:mk_element(Tag,[],[],Pfx);

serialise(integer,Pfx,{Tag,#xmlinfo{},Value}) when is_integer(Value)->
	xmlmisc:mk_element(Tag,[],integer_to_list(Value),Pfx);

serialise(text,Pfx,{Tag,#xmlinfo{},Value}) when is_list(Value)->
	case utils:is_string(Value) of
	true->
		xmlmisc:mk_element(Tag,[],Value,Pfx);

	_->
		?warn({isNotText,Value}),
		[]
	end;

serialise(Other,Pfx,O={Tag,#xmlinfo{},Value})->
	?warn({noserialiser,O}),
	[];

serialise(_Type,Pfx,undefined)->
	[].

getNS()->
	{?PREFIX,?SYSTEMNS}.

getSchema()->
	{?SYSTEMNS,[]}.

getXSL()->
	getXSL(default).

getXSL(_UA)->
	{ok,xmlmisc:xmlxsl([])}.
	
%==============================================================================
% Constructors
%==============================================================================

mk_error(E={error,Reason})->
	mk_error(Reason);

mk_error(Error)->
	#error{description=Error}.

