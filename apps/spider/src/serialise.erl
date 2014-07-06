-module(serialise).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([request/1]).
-export([request/2]).
-export([serialise/2]).


-include("xmlinfo.hrl").

request(Element) when is_tuple(Element)->
	?info({request,Element}),
	case element(2,Element) of
	#xmlinfo{xmlns=XMLNS}->
		%% appears to be a correctly structured object to serialise
		request(XMLNS,Element);

	_Other->
		%% not a correct object, so we will call the objectMapper ....
		?warn({noSerialiser,Element}),
		case objectMapper:map(Element) of
		[]->
			?warn({noObjectMap,Element}),
			[];
		Object->
			?dbug({objectMapper,Element,Object}),
			request(Object)
		end
	end;

request(Other)->
	?warn({noSerialiser,Other}),
	[].

request(XMLNS,Element)->
	case getSerialiser(XMLNS) of
	undefined->
		?warn({noserialiser,XMLNS,Element}),
		[];

	MF={Module,Function}->
		?dbug({getSerialiser,MF,XMLNS}),
		case (catch apply(Module,Function,[Element])) of
		{'EXIT',{undef,_}}->
                        ?warn({undefinedSerialiser,MF}),
                        [];
		E={'EXIT',_Reason}->
                        ?warn({applySerialiser,{object,Element},{mf,MF},E}),
                        [];
                E={error,_Reason}->
                        ?warn({applySerialiser,{object,Element},E}),
                        [];
                XML->
			?dbug({serialisedResult,XML}),
                        XML
		end
	end.

%==============================================================================
% XMLNS to module serialising module mappers
%==============================================================================


getSerialiser("urn:domain")->
	{urn_domain,serialise};

getSerialiser("urn:erlang")->
	{urn_erlang,serialise};

getSerialiser("urn:agent:v1")->
	{urn_agent,serialise};

getSerialiser("urn:controller:v1")->
	{urn_controller,serialise};

getSerialiser("urn:datasource:v1")->
	{urn_datasource,serialise};

getSerialiser("urn:datagatherer:v1")->
	{urn_nstatus,serialise};

getSerialiser("urn:spider:v1")->
	{spider,serialise};

getSerialiser(_Other)->
	undefined.

%==============================================================================
% Standard type serialisers ....
%==============================================================================

serialise(Pfx,[XX=#xstype{}])->
	?dbug({reducingListToTuple,[XX]}),
	serialise(Pfx,XX);

serialise(Pfx,#xstype{tag=Tag,type=integer,value=Value,attributes=A}) when is_integer(Value)->
        xmlmisc:mk_element(Tag,A,integer_to_list(Value),Pfx);

serialise(Pfx,#xstype{tag=Tag,type=null})->
        xmlmisc:mk_element(Tag,[],[],Pfx);

serialise(Pfx,#xstype{tag=Tag,type=text,value=Value,attributes=A})->
	xmlmisc:mk_element(Tag,A,coerce(Value,text),Pfx);

serialise(Pfx,#xstype{tag=Tag,type=ip4address,value={A,B,C,D}})
        when is_integer(A),is_integer(B),is_integer(C),is_integer(D)->
        Value=io_lib:format("~p.~p.~p.~p",[A,B,C,D]),
        xmlmisc:mk_element(Tag,[{type,ip4}],Value,Pfx);

serialise(Pfx,#xstype{tag=Tag,type=erltime,value={A,B,C},attributes=Attributes})
        when is_integer(A),is_integer(B),is_integer(C)->
	TT=calendar:now_to_local_time({A,B,C}),
        Value=xmlmisc:formatUTC(TT),
        xmlmisc:mk_element(Tag,Attributes,Value,Pfx);

serialise(Pfx,#xstype{tag=Tag,type=erlfun,value=Fun}) when is_function(Fun)->
	case (catch erlang:fun_info(Fun,env)) of
        {env,Value=[Function,Module]}->
		?dbug({funInfo,Value}),
		xmlmisc:mk_element(Tag,[{module,Module},{function,Function}],[],Pfx);
	Other->
		?warn({unhandledFunInfo,Other}),
		[]
	end;

serialise(Pfx,#xstype{tag=Tag,type=complex,value=Value,attributes=Attributes})->
	?dbug({serialiseComplex,{value,Value},{attributes,Attributes}}),
	xmlmisc:startXml(Tag,Attributes,Pfx)++
	serialise(Pfx,Value)++
	xmlmisc:endXml(Tag,Pfx);

serialise(Pfx,#xstype{tag=Tag,type=sequence,value=Value,attributes=Attributes})->
	?dbug({serialiseSequence,{value,Value},{attributes,Attributes}}),
	xmlmisc:startXml(Tag,Attributes,Pfx)++
	lists:map(fun(Z)->serialise(Pfx,Z) end,Value) ++
	xmlmisc:endXml(Tag,Pfx);

serialise(_Pfx,Other)->
	?warn({noTypeSerialiser,Other}),
	[].

%==============================================================================
% Utility Routines
%==============================================================================


coerce(Value,text) when is_atom(Value)->
	atom_to_list(Value);

coerce(Value,text) when is_list(Value)->
	case utils:is_string(Value) of
	true->
		Value;
	_->
		?warn({coerceTextFailed,Value}),
		[]
	end;
		
coerce(Value,Type)->
	?warn({noCoerce,Type}),
	Value.
