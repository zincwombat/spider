-module(xmlconfig).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([
	 parse/2,
	 i_parse/3]).


-define(attribute(Name,Value),	#xmlAttribute{name=Name,value=Value}).
-define(element(Name,Attributes,Parent,Content),
	#xmlElement{	name=Name,
			attributes=Attributes,
			parents=Parent,
			content=Content,
			namespace=#xmlNamespace{default=?AGENT_V1_URI}}).

-include("agent.hrl").
-include("urn_agent.hrl").
-include_lib("xmerl/include/xmerl.hrl").


parse(File,UserState={#agentConf{},#controllerConf{},#jobConf{}})->
	PPid=spawn(?MODULE,i_parse,[File,self(),UserState]),
	receive
		{PPid,{close,Message}}->
			?dbug({rcvd,Message}),
			xmerl_scan:user_state(Message);

		E={error,Reason}->
			?warn({parseError,Reason}),
			E;

		Other->
			?warn({unhandledError,Other}),
			Other
			
		after 30000->
			?warn(timeout),
			{error,xmlparse}
	end;

parse(File,X)->
	?error({xmlparse,{badarg,X}}),
	{error,{badarg,X}}.

i_parse(File,Pid,UserState)->
	ParseOpts=[
			{space,normalize},
         		{hook_fun,fun(A,B)->hook(A,B) end},
         		{event_fun,fun(A,B)->event(A,B) end},
         		{user_state,UserState},
         		{close_fun,fun(Z)->Pid ! {self(),{close,Z}} end}],

	case catch xmerl_scan:file(File,ParseOpts) of
	E={error,Reason}->
		?warn({parseError,E}),
		Pid ! E;
	Other->
		?dbug({parseXML,Other})
	end.
	

hook(Entity,GState)->
	NewGState=info(Entity,GState),
	{Entity,NewGState}.
	
event(Entity,GState)->
	NewGState=info(Entity,GState),
	NewGState.

ptree(E,P) when is_list(P)->
	PTree=lists:map(fun(Z)->element(1,Z) end,P),
	XPath=xpath(PTree)++atom_to_list(E),
	XPath.

xpath(PTree)->
	xpath(PTree,[]).

xpath([],Acc)->
	lists:flatten("/"++Acc);

xpath([Elem|Tail],Acc)->
	xpath(Tail,lists:append([atom_to_list(Elem),"/",Acc])).
	

info(E=?element(jobdesc,_,P,C),GState)->
	?info({processingElement,jobdesc}),
	JobDesc=string:strip(get_text(C)),
	?info({jobdesc,JobDesc}),
	{AC,CC,JC}=xmerl_scan:user_state(GState),
	UState={AC,CC,JC#jobConf{jobdesc=JobDesc}},
	xmerl_scan:user_state(UState,GState);

info(E=?element(Name,Attrs,P,_),GState)->
	?info({processingElement,Name}),
	Parents=ptree(Name,P),
	NewGState=attribute(Attrs,Parents,GState),
	NewGState;

info(E=#xmerl_event{event=started,data=document},GState)->
	%% here we should initialise any userState part
	?dbug({startOfDocument,E}),
	UState=xmerl_scan:user_state(GState),
	?dbug({userState,UState}),
	GState;

info(E=#xmerl_event{event=ended,data=document},GState)->
	?dbug(endOfDocument),
	%% here we should finalise any userState part
	?dbug({userStateOnExit,xmerl_scan:user_state(GState)}),
	GState;

info(E=#xmerl_event{event=ended,
		    data=Data=#xmlElement{name=invoke,attributes=A}},GState)->
	%% here we can validate that Invoke has been set correctly
	?dbug({endevent,Data}),

	Module=get_attribute(module,Data),
	Function=get_attribute(function,Data),

	UState=makeFun(invoke,Module,Function,xmerl_scan:user_state(GState)),

	xmerl_scan:user_state(UState,GState);

info(E=#xmerl_event{},GState)->
	GState;

info(Other,GState) when is_tuple(Other)->
	?dbug({processingOther,Other}),
	GState;

info(Other,GState)->
	?dbug({processingOther,Other}),
	GState.

makeFun(invoke,Module,Function,UState={AC=#agentConf{},CC,JC=#jobConf{}})->

	%% need to validate that Module and Function are
	%% both not null, and perhaps more (like that they are
	%% in the code_path etc....

	M=list_to_atom(Module),
	F=list_to_atom(Function),
	?dbug({makeFun,{invoke,M,F}}),
	Invoke=fun(Z)->apply(M,F,[Z]) end,
	{AC#agentConf{invoke=Invoke},CC,JC#jobConf{moduleid=Module,functionid=Function}}.

attribute(AList,Parents,GState) when is_list(AList)->
	UserState=xmerl_scan:user_state(GState),
	NewUState=lists:foldl(fun(Z,Acc)->i_attribute(Parents,Z,Acc) end,UserState,AList),
	xmerl_scan:user_state(NewUState,GState).


%% handle specific attributes

xs(XPath,?attribute(Name,Value))->
	AName=atom_to_list(Name),
	lists:flatten(io_lib:format("~s[@~s='~s']",[XPath,AName,Value])).

%% datasource attributes

i_attribute(XPath="/job/datasource/importfile",
	    E=?attribute(name,FileName),
	    UState={AC,CC,JC=#jobConf{importSpec=IS}})->
	
	FullXPath=lists:flatten(io_lib:format("~s[@~p=~s]",[XPath,name,FileName])),
	?info({setAttribute,xs(XPath,E)}),
	{AC,CC,JC#jobConf{importSpec=IS#import{type=file,
					   value=FileName}}};

i_attribute(XPath="/job/datasource/importfile",
	    E=?attribute(type,Format),
	    UState={AC,CC,JC=#jobConf{importSpec=IS}})->
	?info({setAttribute,xs(XPath,E)}),
	{AC,CC,JC#jobConf{importSpec=IS#import{format=Format}}};

i_attribute(XPath="/job/agent/numagents",
	    E=?attribute(value,Num),
	    UState={AC=#agentConf{},CC=#controllerConf{},JC=#jobConf{}})->
	case coerce(integer,Num) of
	E={error,Reason}->
		?warn({i_attribute,E}),
		throw({error,{badarg,agent,numagents,Num}});
	N->
		?info({setAttribute,xs(XPath,E)}),
		{AC,CC#controllerConf{numagents=N},JC}
	end;

i_attribute(XPath="/job/agent/sleep",
	    E=?attribute(value,S),
	    UState={AC=#agentConf{},CC=#controllerConf{},JC=#jobConf{}})->
	case coerce(integer,S) of
	{error,Reason}->
		throw({error,{badarg,agent,sleep,S}});
	N->
		?info({setAttribute,xs(XPath,E)}),
		{AC#agentConf{sleep=N},CC,JC}
	end;

i_attribute(XPath="/job/agent/timeout",
	    E=?attribute(value,T),
	    UState={AC=#agentConf{},CC=#controllerConf{},JC=#jobConf{}})->
	case coerce(integer,T) of
	{error,Reason}->
		throw({error,{badarg,agent,timeout,T}});
	N->
		?info({setAttribute,xs(XPath,E)}),
		{AC#agentConf{timeout=N},CC,JC}
	end;

i_attribute(XPath="/job/agent/maxrequests",
	    E=?attribute(value,M),
	    UState={AC=#agentConf{},CC=#controllerConf{},JC=#jobConf{}})->
	case coerce(integer,M) of
	{error,Reason}->
		throw({error,{badarg,agent,maxrequests,M}});
	N->
		?info({setAttribute,xs(XPath,E)}),
		{AC#agentConf{max_rq=N},CC,JC}
	end;

i_attribute(XPath="/job/jobid",
	    E=?attribute(value,V),
	    UState={AC,CC,JC=#jobConf{}})->
	?info({setAttribute,xs(XPath,E)}),
	{AC,CC,JC#jobConf{jobid=V}};

i_attribute(XPath="/job/logfile",
	    E=?attribute(name,V),
	    UState={AC,CC,JC=#jobConf{}})->
	?info({setAttribute,xs(XPath,E)}),
	{AC,CC,JC#jobConf{logfile=V}};

i_attribute(XPath="/job/logdir",
	    E=?attribute(name,V),
	    UState={AC,CC,JC=#jobConf{}})->
	?info({setAttribute,xs(XPath,E)}),
	{AC,CC,JC#jobConf{logdir=V}};

i_attribute(XPath,?attribute(A,V),UState)->
	X={A,V},
	?dbug({XPath,{attribute,X}}),
	UState;

i_attribute(Path,Other,UState)->
	?dbug({unhandled,Other}),
	UState.

get_text(#xmlElement{content=C})->
	get_text(C);

get_text(C) when is_list(C)->
	case lists:map(fun(Z)->i_get_text(Z) end,C) of
	[String]->
		String;

	Other->
		Other
	end.

i_get_text(#xmlText{value=Value})->
	Value;

i_get_text(_)->
	[].

get_attribute(Name,#xmlElement{attributes=Attributes})->
	get_attribute(Name,Attributes);

get_attribute(Name,Attributes) when is_list(Attributes)->
	case lists:filter(fun(Z)->findAttribute(Name,Z) end,Attributes) of
	[]->
		{error,notfound};

	[#xmlAttribute{name=Name,value=Value}|Rest]->
		?dbug({findAttribute,{Name,Value}}),
		Value
	end.

%% predicates

findAttribute(Name,#xmlAttribute{name=Name})->
	true;

findAttribute(Name,_)->
	false.
	


config(GroupName,KVList)->
	ok.
	
%% utility routines .......

getAttribute(Name,E=#xmlElement{attributes=A})->
	getAttribute(Name,A);

getAttribute(Name,A) when is_list(A)->
	[FA|_]=lists:filter(fun(Z)->isMatched(Name,Z) end,A),
	getAttribute(Name,FA);

getAttribute(Name,A=#xmlAttribute{name=Name,value=Value})->
	Value;

getAttribute(Name,_)->
	[].

%% type checking and conversion - probably better to use REGEXP

coerce(integer,Val)->
	case catch list_to_integer(Val) of
	{'EXIT',Reason}->
		{error,Reason};
	Num->
		Num
	end.

coerce(String) when is_list(String)->
	Coerced=
	case isType([atom,integer,other],String) of
	[atom|_]->
		list_to_atom(String);
	[integer|_]->
		list_to_integer(String);
	[other]->
		String
	end.

isType(Type,String) when is_list(Type)->
	lists:filter(fun(Z)->isType(Z,String) end,Type);

isType(Type,String) when is_list(String)->
	RegExp=
	case Type of
	integer->
		"^[1-9]+[0-9]*$";
	atom->
		"[a-z][0-9a-zA-Z_]*";
	Other->
		?dbug({unhandledType,Other}),
		"[.]*"
	end,
	case regexp:match(String,RegExp) of
	{match,_,_}->
		true;
	nomatch->
		false
	end.



	

%% useful predicates ..............

isMatched(Name,#xmlAttribute{name=Name})->
	true;

isMatched(_,_)->
	false.

isXMLElement(#xmlElement{})->
	true;

isXMLElement(_)->
	false.

isXMLAttribute(#xmlAttribute{})->
	true;

isXMLAttribute(_)->
	false.

%%

getKVPairs(#xmlElement{name=group,content=C})->
	%% first filter all of the text elements out of the content
	getKVPairs(C);

getKVPairs(C) when is_list(C)->
	FC=lists:filter(fun(Z)->isXMLElement(Z) end,C),
	lists:map(fun(Z)->getKVPairs(Z) end,FC);

getKVPairs(#xmlElement{name=key,attributes=KV})->
	Key=coerce(getAttribute(name,KV)),
	Value=getAttribute(value,KV),
	{Key,Value};

getKVPairs(_)->
	[].

