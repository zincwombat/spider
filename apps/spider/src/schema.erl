-module(schema).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").


-export([schemaLoc/1,
	 schema/1,
	 x_assemble/3,
	 assemble/2,
	 element/1,
	 getValue/1,
	 getValue/2,
	 element2Object/1,
	 addAttribute/1,
	 addAttribute/3,
	 getAttribute/2,
	 loadAttributes/3,
	 contentType/1,
	 constructor/1,
	 object/1,
	 objectModel/1,
	 objectModel/2,
	 contentModel/1]).

-include("schema.hrl").
-include("xml.hrl").
-include("schemaloc.hrl").

schemaLoc({object,Object})->
        schemaLoc(object:getVersion(Object));

schemaLoc(#nsinfo{uri=URI})->
        %% using either a #nsinfo{} or #version{} as the key, return
        %% the name of the module that contains the serialisation mapping
        i_schemaLoc(URI);

schemaLoc(#version{version=URI})->
        i_schemaLoc(URI);

schemaLoc(#elementSchema{nsinfo={URI,Prefix}})->
        i_schemaLoc(URI);

schemaLoc(Object) when is_tuple(Object)->
	schemaLoc(object:getVersion(Object));

schemaLoc(URI)->
	i_schemaLoc(URI);

schemaLoc(Other)->
        ?error({no_schema_location,Other}).

contentType(O={object,Object})->
	case schemaLoc(O) of
	{schemaLoc,SchemaLoc}->
		apply(SchemaLoc,contentModel,[O]);
	Error->
		Error
	end;

contentType(Object) when is_tuple(Object)->
	contentType({object,Object}).


getValue(Context)->
	case schemaLoc(Context) of
	{schemaLoc,SchemaLoc}->
		apply(SchemaLoc,getValue,[Context]);
	Error->
		Error
	end.


getValue(Context,Child)->
	case schemaLoc(Context) of
	{schemaLoc,SchemaLoc}->
		i_getValue(SchemaLoc,Context,Child);
	Error->
		throw(Error)
	end.

i_getValue(SchemaLoc,#elementSchema{name=E},Object)->
	i_getValue(SchemaLoc,E,Object);

i_getValue(SchemaLoc,{object,Context},{object,Object})->
	i_getValue(SchemaLoc,Context,Object);

i_getValue(SchemaLoc,Context,Object)->
	apply(SchemaLoc,getValue,[Context,Object]).

getAttribute(Attribute,Object)->
	case schemaLoc(Object) of
	{schemaLoc,SchemaLoc}->
		apply(SchemaLoc,getAttribute,[Attribute,Object]);
	Error->
		throw(Error)
	end.

loadAttributes(Element,Attributes,Object)->
	case schemaLoc(Object) of
	{schemaLoc,SchemaLoc}->
                apply(SchemaLoc,loadAttributes,[Element,Attributes,Object]);
        Error->
                throw(Error)
        end.


element(Object)->
	case schemaLoc(Object) of
	{schemaLoc,SchemaLoc}->
		i_element(SchemaLoc,Object);
	Error->
		throw(Error)
	end.

i_element(SchemaLoc,{object,Object})->
	i_element(SchemaLoc,Object);

i_element(SchemaLoc,Object) when is_tuple(Object)->
	i_element(SchemaLoc,element(1,Object));

i_element(SchemaLoc,Object)->
	apply(SchemaLoc,element,[Object]).

schema(Object)->
	case schemaLoc(Object) of
        {schemaLoc,SchemaLoc}->
                i_schema(SchemaLoc,Object);
        Error->
                throw(Error)
        end.

i_schema(SchemaLoc,{object,Object})->
	i_schema(SchemaLoc,Object);

i_schema(SchemaLoc,N=#nsinfo{element=E})->
	i_schema(SchemaLoc,list_to_atom(E));

i_schema(SchemaLoc,Object) when is_tuple(Object)->
	i_schema(SchemaLoc,element(1,Object));

i_schema(SchemaLoc,Object)->
	apply(SchemaLoc,schema,[Object]).

object(Element)->
	case schemaLoc(Element) of
	{schemaLoc,SchemaLoc}->
		i_object(SchemaLoc,Element);
	Error->
		throw(Error)
	end.

i_object(SchemaLoc,ES=#elementSchema{name=E})->
	i_object(SchemaLoc,E);

i_object(SchemaLoc,NS=#nsinfo{element=E})->
	i_object(SchemaLoc,E);

i_object(SchemaLoc,E) when is_list(E),is_integer(hd(E))->
	i_object(SchemaLoc,list_to_atom(E));

i_object(SchemaLoc,Element)->
	apply(SchemaLoc,object,[Element]).

addAttribute(O={object,Object})->
	case schema(O) of
	S when record(S,schema)->
		case S#schema.attributeConstructor of
		F when function(F)->
			F;
		_->
			throw({error,{no_attribute_constructor,element(1,Object)}})
		end;

	Other->
		throw({error,{no_schema,{element(1,Object),Other}}})
	end;

addAttribute(Object) when is_tuple(Object)->
	addAttribute({object,Object}).

%% --

addAttribute(Model,Attribute,Object={object,O}) when is_list(Attribute)->
        lists:foldl(fun(Z,Acc)->addAttribute(Model,Z,Acc) end,Object,Attribute);

addAttribute(Model,Attribute,Object={object,O})->
        i_addAttribute(Model,Attribute,Object).

i_addAttribute([AS=#attrSchema{name=A}|_],{attribute,AT={A,_}},Object={object,O})->
        i_addAttribute(AS,AT,Object);

i_addAttribute([AS|Rest],A,Object={object,O})->
        i_addAttribute(Rest,A,Object);

i_addAttribute([],A,Object={object,O})->
        Object;

i_addAttribute(AS=#attrSchema{},A={Name,Value},Object={object,O})->
        VA=validateAttribute(AS,A),
        loadAttributes('__direct__',A,Object).

validateAttribute(#attrSchema{},A)->
        A.



constructor(O={object,Object})->
	case schema(O) of
	S when record(S,schema)->
		S#schema.constructor;

	Other->
		Other
	end;

constructor(Object) when is_tuple(Object)->
	constructor({object,Object}).

objectModel(O)->
	case contentModel(O) of
	ES=[#elementSchema{}|_]->
		lists:map(fun(Z)->element2Object(Z) end,ES);

	Other->
		Other
	end.

objectModel(Parent,Child)->
	case objectModel(Parent) of
	OM when list(OM)->
		searchObjectModel(OM,Child);
	Other->
		Other
	end.

searchObjectModel([{{object,Child},Min,Max}|_],Child)->
	{Min,Max};

searchObjectModel([{{object,Other},Min,Max}|Rest],Child)->
        searchObjectModel(Rest,Child);

searchObjectModel([],Child)->
	[].


contentModel(O={object,Object})->
	case schema(O) of
	S when record(S,schema)->
		S#schema.content;

	Other->
		Other
	end;

contentModel(Object) when is_tuple(Object)->
	contentModel({object,Object}).

element2Object(Element=#elementSchema{name=E,nsinfo={URI,_},minoccurs=Min,maxoccurs=Max})->
	case schemaLoc(Element) of
	{schemaLoc,SchemaLoc}->
		Object=apply(SchemaLoc,object,[#nsinfo{uri=URI,element=E}]),
		{Object,Min,Max};
	Other->
		Other
	end.

sequence(Object,Children)->
        lists:map(fun(Z)->assemble(Object,Z) end,Children).

complex(Object,Children)->
        lists:foldl(fun(Z,Acc)->assemble(Acc,Z) end,Object,Children).


x_assemble(Model,Pa,Cs)->
        %% which object does Parent correspond to ??
        %% now convert this to the object model
        Parent=object(Pa),
        ObjectModel=objectModel(Model),
        P=
        case ObjectModel of
        [{Object,0,1}]->
                assemble(Parent,Cs);

        [{Object,1,1}]->
                assemble(Parent,Cs);

        [{Object,Min,Max}]->
                complex(Parent,Cs);

        List when is_list(List)->
                complex(Parent,Cs);

        simple when list(Cs),not is_integer(hd(Cs))->
                sequence(Parent,Cs);

        simple->
                assemble(Parent,Cs);

        Other->
                throw(?error({unhandled,Other}))
        end.


assemble(O={object,Object},C) when list(C), not is_integer(hd(C))->
	lists:foldl(fun(Z,Acc)->assemble(Acc,Z) end,O,C);

assemble(O={object,Object},C)->
	case schemaLoc(O) of
	{schemaLoc,SchemaLoc}->
		apply(SchemaLoc,assemble,[O,C]);
	Other->
		throw(Other)
	end;

assemble(Object,Child)->
	assemble({object,Object},Child).
