-module(urn_agent).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([serialise/1,
	 getNS/0,
	 getSchema/0,
	 getXSL/0,
	 getXSL/1]).


-include("urn_agent.hrl").
-sysloglevel(?TRACE_WARN).

-define(THIS_XMLNS,	xmlinfo=?XML(?URN_AG)).

%%=============================================================================
%% Root Element Serialiser
%%=============================================================================

serialise(AGI=#ag_info{?THIS_XMLNS})->
        XML=serialise(complex,?URN_AG_PFX,AGI),
        lists:flatten(XML);

serialise(AGP=#ag_payload{?THIS_XMLNS})->
        XML=serialise(complex,?URN_AG_PFX,AGP),
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

serialise(Pfx,M=#timeout{?THIS_XMLNS})->
	serialise(integer,Pfx,M);

serialise(Pfx,M=#max_rq{?THIS_XMLNS})->
	serialise(integer,Pfx,M);

serialise(Pfx,M=#sleep{?THIS_XMLNS})->
	serialise(integer,Pfx,M);

serialise(_,undefined)->
	[];

serialise(_,P) when is_tuple(P)->
	case element(2,P) of
	#xmlinfo{xmlns=?URN_AG}->
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

getAttributes(_)->
	[].


serialise(_Type,Pfx,{Tag,#xmlinfo{},undefined})->
	xmlmisc:mk_element(Tag,[],[],Pfx);

serialise(complex,Pfx,Complex) when is_tuple(Complex)->
        case tuple_to_list(Complex) of
        [Tag,#xmlinfo{xmlns=?URN_AG}|Content]->
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

serialise(erlangtime,Pfx,{Tag,#xmlinfo{},T={_A,_B,_C}})->
	TT=calendar:now_to_local_time(T),
	TTX=xmlmisc:formatUTC(TT),
	xmlmisc:mk_element(Tag,[],TTX,Pfx);

serialise(_Other,_Pfx,O={_Tag,#xmlinfo{},_Value})->
	?warn({noserialiser,O}),
	[];

serialise(_Type,_Pfx,undefined)->
	[].

%%=============================================================================
%% CONSTRUCTORS
%%=============================================================================

getNS()->
	{?URN_AG_PFX,?URN_AG}.

getSchema()->
	{?URN_AG,[]}.

getXSL()->
	getXSL(default).

getXSL(_UA)->
	{ok,[]}.
	

	
