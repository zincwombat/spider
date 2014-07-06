-module(urn_controller).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([serialise/1,
	 getNS/0,
	 getSchema/0,
	 getXSL/0,
	 getXSL/1]).


-include("controller.hrl").
-include("agent.hrl").

-sysloglevel(?TRACE_WARN).
-define(THIS_XMLNS,	xmlinfo=?XML(?URN_CC)).

%%=============================================================================
%% Root Element Serialiser
%%=============================================================================

serialise(CCI=#cc_info{?THIS_XMLNS})->
	XML=serialise(complex,?URN_CC_PFX,CCI),
	lists:flatten(XML);

serialise(Other)->
	?warn({noserialiser,Other}),
	[].

%%=============================================================================
%% Content Serialisers
%%=============================================================================

serialise(Pfx,T=#xstype{})->
	?dbug({callingTypeSerialise,T}),
	serialise:serialise(Pfx,T);

serialise(Pfx,#cc_agentConf{?THIS_XMLNS,agentConf=AC})->
	%% these are complex types
	serialise(Pfx,AC);

serialise(Pfx,#cc_controllerConf{?THIS_XMLNS,controllerConf=CC})->
	serialise(Pfx,CC);

serialise(Pfx,CCF=#cc_conf{?THIS_XMLNS})->
	serialise(complex,Pfx,CCF);

serialise(_,undefined)->
	[];

serialise(Pfx,P) when is_tuple(P)->
	case element(2,P) of
	#xmlinfo{xmlns=?URN_CC}->
		?dbug({noSerialiserThisXmlns,P}),
		[];

	#xmlinfo{xmlns=XMLNS}->
		?dbug({externalSerialiser,{xmlns,XMLNS},{object,element(1,P)}}),
		serialise:request(XMLNS,P);

	_Other->
		?dbug({noMatch,P}),
		%% we should call the objectMapper just in case ....
		case objectMapper:map(P) of
		[]->
			?dbug({noSerialiser,P}),
			[];
		NewObject->
			?dbug({objectMapperReturn,NewObject}),
			serialise(Pfx,NewObject)
		end
	end;

serialise(_,P)->
	?dbug({noSerialiser,P}),
	[].

getAttributes(_)->
	[].

%==============================================================================
% Complex content type serialisers
%==============================================================================

serialise(_Type,Pfx,{Tag,#xmlinfo{},undefined})->
	xmlmisc:mk_element(Tag,[],[],Pfx);

serialise(complex,Pfx,Complex) when is_tuple(Complex)->
	case tuple_to_list(Complex) of
	[Tag,#xmlinfo{xmlns=?URN_CC}|Content]->
		%% correct namespace
		?dbug({serialisingComplexType,Complex}),
		Attributes=getAttributes(Complex),
		xmlmisc:startXml(Tag,Attributes,Pfx) ++
		lists:map(fun(Z)->serialise(Pfx,Z) end,Content) ++
		xmlmisc:endXml(Tag,Pfx);
	
	_Other->
		?warn({complexNoSerialiser,Complex}),
		[]
	end;


%==============================================================================
% Simple content type serialisers
%==============================================================================

serialise(_Other,_Pfx,O={_Tag,#xmlinfo{},_Value})->
	?warn({noserialiser,O}),
	[];

serialise(_Type,_Pfx,undefined)->
	[].

%%=============================================================================
%% CONSTRUCTORS
%%=============================================================================

getNS()->
	{?URN_CC_PFX,?URN_CC}.

getSchema()->
	{?URN_CC,[]}.

getXSL()->
	getXSL(default).

getXSL(_UA)->
	{ok,[]}.
	

	
