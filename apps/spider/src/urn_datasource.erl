-module(urn_datasource).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([serialise/1,
	 getNS/0,
	 getSchema/0,
	 getXSL/0,
	 getXSL/1]).


-include("agent.hrl").
-include("datasource.hrl").
-sysloglevel(?TRACE_WARN).

-define(THIS_XMLNS,	xmlinfo=?XML(?URN_DS)).

%%=============================================================================
%% Root Element Serialiser
%%=============================================================================

serialise(DGI=#ds_info{?THIS_XMLNS})->
        XML=serialise(complex,?URN_DS_PFX,DGI),
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

serialise(Pfx,N=#next{?THIS_XMLNS,next='$end_of_table'})->
	serialise(Pfx,N#next{next="***DATA SOURCE EXHAUSTED***"});

serialise(Pfx,IS=#import_spec{?THIS_XMLNS,import_spec=#import{type=T,format=F,value=V}})->
	Attributes=[{type,T},{format,F}],
	xmlmisc:mk_element(import_data,Attributes,V,Pfx);

serialise(_,undefined)->
	[];

serialise(_,P) when is_tuple(P)->
	case element(2,P) of
	#xmlinfo{xmlns=?URN_DS}->
		?dbug({noSerialiserThisXmlns,P}),
		[];

	#xmlinfo{xmlns=XMLNS}->
		?dbug({externalSerialiser,{xmlns,XMLNS},{object,element(1,P)}}),
		serialise:request(XMLNS,P);

	_Other->
		?dbug({noSerialiser,P}),
		[]
	end;

serialise(_,P)->
	?dbug({noSerialiser,P}),
	[].

%==============================================================================
% Simple content type serialisers
%==============================================================================

getAttributes(_)->
	[].

serialise(_Type,Pfx,{Tag,#xmlinfo{},undefined})->
	xmlmisc:mk_element(Tag,[],[],Pfx);

serialise(complex,Pfx,Complex) when is_tuple(Complex)->
        case tuple_to_list(Complex) of
        [Tag,#xmlinfo{xmlns=?URN_DS}|Content]->
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

serialise(_Other,_Pfx,O={_Tag,#xmlinfo{},_Value})->
	?warn({noserialiser,O}),
	[];

serialise(_Type,_Pfx,undefined)->
	[].

%%=============================================================================
%% CONSTRUCTORS
%%=============================================================================

getNS()->
	{?URN_DS_PFX,?URN_DS}.

getSchema()->
	{?URN_DS,?SCHEMALOC}.

getXSL()->
	getXSL(default).

getXSL(_UA)->
	{ok,xmlmisc:xmlxsl(?XSL)}.
	

	
