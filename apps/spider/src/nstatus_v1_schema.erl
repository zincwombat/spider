-module(nstatus_v1_schema).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([schema/1,
	 element/1,
	 getValue/1,
	 getValue/2,
	 getAttribute/2,
	 loadAttributes/3,
	 assemble/2,
	 object/1]).

-export([test/0]).

-include("xml.hrl").
-include("schema.hrl").
-include("nstatus.hrl").
-include("cmanager.hrl").


test()->
	ok.

%% ---
elementSchema(Name,Min,Max)->
        #elementSchema{name=Name,
                       nsinfo={?NSTATUS_V1_URI,"nstatus"},
                       minoccurs=Min,
                       maxoccurs=Max}.


attrSchema(Name,Type,Use,Default)->
        #attrSchema{name=Name,
                    type=Type,
                    use=Use,
                    default=Default}.

attrSchema(Name,Type,Use)->
        #attrSchema{name=Name,
                    type=Type,
                    use=Use}.


element(Object) when is_tuple(Object)->
	element(element(1,Object));

%%============================================================================= 
%% Object to Element mapping 
%%============================================================================= 

element({object,Object})->
        element(Object);

element(Object) when is_tuple(Object)->
        element(element(1,Object));


element(Object) when atom(Object)->
	Element=atom_to_list(Object),
	%% lets also return the content model
	Schema=
	case schema(Element) of
	S when record(S,schema)->
		S;
	_->
		[]
	end,
	#nsinfo{uri=?NSTATUS_V1_URI,
		prefix="ns",
		element=Element,
		cmodel=Schema};

element(Other)->
	{error,{element,Other}}.

%%============================================================================= 
%% Element Schema Definitions - returns a #schema{} record as defined in
%% schema.hrl
%%============================================================================= 

schema({object,Object})->
        schema(element(Object));

schema(NSInfo) when record(NSInfo,nsinfo)->
	schema(NSInfo#nsinfo.element);

schema(Element) when list(Element)->
	schema(list_to_atom(Element));

schema(Element) when atom(Element)->
        #i_schema{attributes=A,model=M,content=C}=i_schema(Element),
        #schema{name=Element,
                xmlns=?NSTATUS_V1_URI,
                attributes=A,
                model=M,
                constructor=fun(Z)->x_assemble(C,Element,Z) end,
                attributeConstructor=fun(Z,G)->addAttribute(A,Z,G) end,
                content=C}.

%%============================================================================= 
%% i_schema simply returns the contentType, contentModel and attribute Model
%% for each element
%%=============================================================================

i_schema(nstatus)-> 
	#i_schema{
		model=complex,
		attributes=[attrSchema(code,string,optional,9999),
			    attrSchema(module,string,optional),
			    attrSchema(line,string,optional)],
		content=[elementSchema('cm_state',1,1),
			 elementSchema('cc_state',1,1),
			 elementSchema('ds_status',1,1)]};

i_schema(cm_state)->
	#i_schema{
                model=complex,
                content=[elementSchema('cstate',1,1),
                         elementSchema('datasource',1,1),
                         elementSchema('configfile',1,1)]};


i_schema(Other)->
	throw({error,{no_schema,Other}}).

%%============================================================================= 
%% object/1 returns an object definition associated with a particular Element
%% for each element
%%=============================================================================

object(#elementSchema{name=E})->
        object(E);

object(#nsinfo{element=E})->
	object(E);

object(Element) when list(Element),is_integer(hd(Element))->
	object(list_to_atom(Element));

object(nstatus)->
	{object,#nstatus{}};

object(cm_state)->
	{object,#cm_state{}};

object(Object)->
	throw(?error({no_object,Object})).

%% getValue implementation - 

%% returns:	{simple,Tag,Attributes,Content} - for simple types
%%		{complex,Tag,Attributes,NextObject} - for complex types
%%		{sequence,NextObjects} - for sequences
%% TBD - mixed types not handled
%%=============================================================================
%% getValue/2 is used to disassemble an object into its content. It is used
%% by serialise
%%=============================================================================

getValue({object,ContextObject})->
	getValue(ContextObject);

getValue(CX=#nstatus{})->
	{object,CX};

getValue(Object)->
	throw(?error({no_object,Object})).

getValue({object,ContextObject},{object,Object})->
	getValue(ContextObject,Object);

getValue(#elementSchema{name=E},Object)->
	getValue(E,Object);
	
getValue(CX=#nstatus{},Object=#nstatus{})->
	{ok,CX};

getValue(Type,Object)->
	throw(?error({no_object,Type,Object})).

osig(Object)->
	object:signature(Object).

%% process the Element attributes into the Object

getAttribute(Name,{object,Object})->
	getAttribute(Name,Object);

%%getAttribute(line,#erlerror{line=L}) when L/=undefined->
%%	{line,L};

getAttribute(Other,#nstatus{})->
	?UNDEF_ATTR.

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

loadAttributes(Element,Attributes,O={object,Object}) when list(Attributes)->
	lists:foldl(fun(Z,Acc)->loadAttributes(Element,Z,Acc) end,O,Attributes);

loadAttributes(Element,A={Name,Value},O={object,Object})->
	O.

%% - assembly routines, necessary when an object is decomposed into subordinate objects

objectModel(simple)->
        simple;

objectModel(ES) when is_list(ES)->
        lists:map(fun(Z)->objectModel(Z) end,ES);

objectModel(#elementSchema{name=E,nsinfo={URI,_},minoccurs=Min,maxoccurs=Max})->
        {object(E),Min,Max}.

sequence(Object,Children)->
        lists:map(fun(Z)->assemble(Object,Z) end,Children).

complex(Object,Children)->
        lists:foldl(fun(Z,Acc)->assemble(Acc,Z) end,Object,Children).


%% - assembly routines, necessary when an object is decomposed into subordinate objects

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


assemble({object,Parent},{object,Child})->
	assemble(Parent,Child);

%assemble(E=#erlerror{},D=#erlerrordetail{})->
%	{object,E#erlerror{detail=D}};

assemble(Parent,Child)->
	 throw(?error({cannot_assemble,{element(1,Child),into,element(1,Parent)}})).




