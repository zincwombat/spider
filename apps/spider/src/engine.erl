-module(engine).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([out/1]).
-export([rpc/3]).

-include("yaws_api.hrl").
-include("agent.hrl").
-include("controller.hrl").
-include("cmanager.hrl").
-include("analysis.hrl").
-include("filter.hrl").
-include("rlogger.hrl").
-define(COMMENT,		"<!-- comment -->").
-define(XMLPI,			"<?xml version=\"1.0\" encoding=\"UTF-8\" ?>").
-define(DOCTYPE,		"<!DOCTYPE html>").
-define(TIMEOUT,		30000).	%% timeout per request
-define(STYLE,			"/css/spider.css").
-define(DEFAULT_TRACE_LEVEL,	1).
-define(TRACE_ONLY_TAG,		"__TRACE_ONLY__").

-define(MENU_MAIN,		{"Main",	"/engine"}).
-define(MENU_CONF,		{"Set Analysis","/engine/configure"}).
-define(MENU_FILTER,		{"Set Filter",	"/engine/filter"}).
-define(MENU_START,		{"Start",	"/engine/start"}).
-define(MENU_STOP,		{"Stop",	"/engine/stop"}).
-define(MENU_RESUME,		{"Resume",	"/engine/resume"}).
-define(MENU_SUSPEND,		{"Suspend",	"/engine/suspend"}).
-define(MENU_SYSLOG,		{"Syslog",	"/engine/syslogconf"}).
-define(MENU_LOADFILE,		{"Import File",	"/engine/loadfile"}).
-define(MENU_LOADLIST,		{"Import List",	"/engine/loadlist"}).
-define(MENU_DATASTATUS,	{"Data Status",	"/engine/datastatus"}).
-define(MENU_PURGE,		{"Purge Data",	"/engine/purge"}).

-record(state,			{cm,cc,io_data}).
-record(upload, 		{fd,filename,last}).	%% for file uploads

%% TODO -- UDIR should be defined in a global include file

-define(UDIR,			"/home/pi/spider/upload/").	%% upload directory
			
-define(format_record(Rec,Name), format_record(Rec,Name,record_info(fields,Name))).


%% - HTTP interface handler 

out(Arg) when record(Arg,arg)->

        Method=(Arg#arg.req)#http_request.method,
        Path=Arg#arg.appmoddata,
	UserAgent=(Arg#arg.headers)#headers.user_agent,

	case (catch cmanager:status()) of
	{ok,{{manager,DState=#cm_state{}},{controller,CS}}}->

		State=#state{cm=DState,io_data=[]},

		?dbug({userAgent,UserAgent}),
		?dbug({method,Method}),
		?dbug({path,Path}),

        	case {Method,Path} of
        	{'GET',Path}->
			QS=Arg#arg.querydata,
			QSP=yaws_api:parse_query(Arg),
                	?dbug({parsedS,QSP}),
			handle(get,Path,QSP,State,Arg);

		{'POST',"upload"}->
			handle_upload(Arg);

        	{'POST',Path}->
			QSP=yaws_api:parse_post(Arg),
			?dbug({postQS,QSP}),
			handle(post,Path,QSP,State,Arg);

        	Other->
                	%% any other post requests generate a server error
                	?dbug({pathNotRecognised,Other}),
                	{status,500}
        	end;

	Error->
		?critical(Error),
		{status,500}
	end.

handle(get,"",QSP,State,AState)->
	page([]);

handle(get,"syslogconf",QSP,State,AState)->
	?info({get,logger,QSP}),
	page(syslogger());

handle(get,"setAgents",QSP,State,AState)->
	?info({get,setAgents,QSP}),
	NumAgents=
	case getParam("num",QSP) of
	[]->
		?DEFAGENTS;
	N->
		N
	end,
	page(setAgents(NumAgents));

handle(get,"loadfile",QSP,State=#state{cm=#cm_state{cstate='IDLE'}},AState)->
	?info({get,loadfile,QSP}),
	page(uploadFile());

handle(get,"datastatus",QSP,State,AState)->
	?info({get,datastatus,QSP}),
	Result=datasource:status(),
	page(html(Result));

handle(get,"loadlist",QSP,State=#state{cm=#cm_state{cstate='IDLE'}},AState)->
	?info({get,loadlist,QSP}),
	page(importList());

handle(get,"configure",QSP,State=#state{cm=#cm_state{cstate='IDLE'}},AState)->
	?info({get,configure,QSP}),
	page(configure(State));

handle(get,"filter",QSP,State=#state{cm=#cm_state{cstate='IDLE'}},AState)->
	%% add some additional state qualification here ! TBD
	?info({get,configure,QSP}),
	page(setFilter(State));

handle(get,"purge",QSP,State=#state{cm=#cm_state{cstate='IDLE',datasource=ready}},AState)->
	?info({get,purge,QSP}),
	Result=cmanager:purgeData(),
	page([html(Result)]);

handle(get,"start",QSP,State=#state{cm=#cm_state{cstate='IDLE'}},AState)->
	?info({get,start,QSP}),
	Result=cmanager:run(),
	page([html(Result)]);

handle(get,"stop",QSP,State,AState)->
	?info({get,stop,QSP}),
	Result=cmanager:reset(),
	page([html(Result)]);

handle(get,"output",QSP,State,AState)->
	?info({get,output,QSP}),
	case (catch cmanager:getOutputFile()) of
	{ok,F}->
		%% construct the relative path name
		NewPath="/"++?YAWS_ANALYSIS_DIR++"/"++F,
		{redirect,NewPath};
	Other->
		page([html(Other)])
	end;

	
handle(get,"menu",QSP,State=#state{cm=#cm_state{cstate=CState}},AState)->
	%% called by AJAX /jquery
	{ehtml,menu(State)};

handle(get,"syslogtable",QSP,State,AState)->
	%% called by AJAX /jquery
	T=syslogger(),
	{ehtml,T};

handle(get,"writestats",QSP,State,AState)->
	%% called by AJAX /jquery
	T=i_dynamicOp(writer,State),
	{ehtml,T};

handle(get,Other,QSP,State,AState)->
	?dbug({get,{catchall,Other},QSP}),
	page([{p,[],"unimplemented"}]);

handle(get,Path,QSP,State,AState)->
	?info({get_unhandled,Path,QSP}),
	{status,404};


handle(post,"importdatalist",QSP,State,AState)->
	DomainList=extract("domainlist",QSP),
	case DomainList of
	[{"domainlist",D}] when list(D)->
                {_,PDomains}=tokenise(D),

		%% now we need to load this into the datasource ....
		%% need to wrap this in a catch .....

		Reply=(catch cmanager:importList(PDomains)),
		?dbug({datasourceReply,Reply}),
		page([html(Reply)]);

	[{"domainlist",undefined}]->
		?warn(noDomains),
		{status,404}
	end;


handle(post,Path="importdatafile",QSP,State,AState)->
	%% need to extract the parameters filename and filetype
	FileName=getParam("filename",QSP),
	FileType=getParam("filetype",QSP),
	Reply=
	case (catch cmanager:setImportFile(FileName,FileType)) of
	ok->
		(catch cmanager:import());
	Other->
		Other
	end,
	page([html(Reply)]);

handle(post,Path="setAgents",QSP,State,AState)->
	%% need to extract the parameters filename and filetype
	NumAgents=getParam("numagents",QSP),
	Reply=(catch cmanager:setNumAgents(NumAgents)),
	page([html(Reply)]);


handle(post,Path="upload",QSP,State,AState)->
	?info({handling_upload,{qsp,QSP},{state,State},{astate,AState}}),
	%% need to extract the parameters filename and filetype
	Reply=multipart(AState,#upload{}),
	case Reply of
	M={get_more,Cont,NewState}->
		M;

	{ok,{uploaded,FileName}}->
		Z=
		case (catch cmanager:setImportFile(FileName,"csv")) of
		ok->
			(catch cmanager:import());
		Other->
			Other
		end,
		page([html(Z)]);
		

	Other2->
		Other2,
		page([html(Other2)])
	end;

handle(post,Path="configure",QSP,State,AState)->

	%% extract the following parameters
	%% description
	%% email
	%% moduleid
	%% functionid
	%% agents

	Description=getParam("description",QSP),
	Email=getParam("email",QSP),
	Agents=getParam("agents",QSP,integer_to_list(?DEFAGENTS)),
	Filter=getParam("filter",QSP), %% need a default here !!
	Key=getParam("analysis",QSP),

	R1={jobDescription,(catch cmanager:setJobDescription(Description))},
	R2={numAgents,(catch cmanager:setNumAgents(list_to_integer(Agents)))},
	R3={email,(catch cmanager:setEmail(Email))},
	
	case Key of
	[]->
		undefined;
	N->
		NI=list_to_integer(N),
		case lists:keysearch(NI,2,?ANALYSIS) of
		{value,A=#analysis_opt{module=M,function=F}}->
			?dbug({module,M,function,F}),
			cmanager:setModuleFunction(M,F,NI);
					

		Other->
			?dbug({Other,NI,?ANALYSIS})
		end
	end,

	{ok,JC}=cmanager:getJobConf(),
	{ok,CC}=cmanager:getControllerConf(),
	{ok,AC}=cmanager:getAgentConf(),

	Reply={ok,[?format_record(JC,jobConf),
		  ?format_record(CC,controllerConf),
		  ?format_record(AC,agentConf)]},

	page(html(Reply));

handle(post,Path="setFilter",QSP,State,AState)->

	%% set the output filter

	Reply=
	case getParam("filter",QSP) of
	[]->
		{error,nofilter};
	F->
		(catch cmanager:setFilter(list_to_atom(F)))
		%% we should also change the file extension if we know
		%% it to be xml or other
	end,
	page(html(Reply));

handle(post,Path="json",QSP,State,AState)->
	%% this is a JSON RPC request and we need to use the
	%% RPC handler and pass AState to it
	?dbug({json,Path,QSP}),

	%% need to put a catch around this and handle errors

	yaws_rpc:handler_session(AState,{?MODULE,rpc});

handle(post,Path=Unhandled,QSP,State,AState)->
	?info({post_unhandled,Path,QSP}),
	{status,200}.


handle_upload(Arg) when Arg#arg.state==undefined->
	handle_upload(Arg#arg{state=#upload{}});

handle_upload(Arg)->
	Reply=
	case multipart(Arg,Arg#arg.state) of
	M={get_more,Cont,NewState}->
		M;
	{ok,{uploaded,FileName}}->
		R1=
		case (catch cmanager:setImportFile([?UDIR,FileName],"csv")) of
		ok->
			(catch cmanager:import());
		Other->
			Other
		end,
		page([html(R1)]);

	Other2->
		Other2,
		page([html(Other2)])
	end.


%% JSON RPC Handling ......

rpc(State,Request={call,systatus,{array,[]}},Session)->
	?dbug({rpc_request,{state,State},{request,Request},{session,Session}}),
	{ok,Cs={cstate,CState}}=cmanager:getCState(),
	W=
	case rlogger:getWriteStats() of
	N when is_integer(N)->
		N;
	_->
		0
	end,
	Response={response,{struct,[{cstate,atom_to_list(CState)},{writes,W}]}},
	?dbug({rpc_response,Response}),
	{false,Response};

rpc(State,Request={call,getmenu,{array,[CurState]}},Session)->
	?dbug({rpc_request,{state,State},{request,Request},{session,Session}}),
	Response={response,"new menu here!"},
	?dbug({rpc_response,Response}),
	{false,Response};

rpc(State,Request={call,syslog,{array,[?TRACE_ONLY_TAG,Module]}},Session)->
	?info({rpc_request,{state,State},{request,Request},{session,Session}}),

	Only=syslogger:getOnly(),
	M=list_to_atom(Module),

	R=
	case Only of
	[]->
		(catch syslogger:setOnly(M));

	M->
		(catch syslogger:all());

	Other->
		(catch syslogger:setOnly(M))
	end,
	
	Response={response,atom_to_list(R)},
	?info({rpc_response,Response}),
	{false,Response};

rpc(State,Request={call,syslog,{array,[Module,Level]}},Session)->
	?dbug({rpc_request,{state,State},{request,Request},{session,Session}}),
	R=
	case (catch syslogger:set(Module,Level)) of
	true->
		"success";
	Other->
		"failure"
	end,
	Response={response,R},
	?dbug({rpc_response,Response}),
	{false,Response};

rpc(State,Request=Unhandled,_Session)->
	?warn({unhandledRPC,Unhandled}),
	{error,{unhandledRPC,Unhandled}}.

%% see if we can replace this with syslogger or include macros.....

%%=============================================================================
%% miscellaneous functions
%%=============================================================================

extractFilter(Filters) when is_list(Filters)->
        lists:map(fun({filter,Z})->list_to_atom(Z) end,Filters).
                                                                                
extract(Name,QS)->
        extract(Name,QS,[]).
                                                                                
extract(Name,QS,Default)->
        N=lists:filter(fun(Z)->is_it(Name,Z) end,QS),
        case N of
        []->
                [{Name,Default}];
        _->
                N
        end.
                                                                                
is_it(Name,{Name,_})->
        true;
                                                                                
is_it(Name,{_,_})->
        false.

	
tokenise(All)->
        tokenise(out,All,[]).
                                                                                
tokenise(_,[],L)->
        {[],lists:reverse(L)};
                                                                                
tokenise(out,All=[H|T],L) ->
        %% we are currently not processing a token
        case is_domainChar(H) of
        true->
                %% start processing a token
                Q={Rest,Token}=tokenise(in,All,[]),
                tokenise(out,Rest,[Token|L]);
        false->
                tokenise(out,T,L)
        end;
                                                                                
tokenise(in,All=[H|T],L)->
        case is_domainChar(H) of
        true->
                tokenise(in,T,[H|L]);
        false->
                {T,lists:reverse(L)}
        end.

%%=============================================================================
%% multidomain tokenising routines
%%=============================================================================
                                                                                
is_domainChar(X) when X >= $a, X =< $z -> true;
is_domainChar(X) when X >= $A, X =< $Z -> true;
is_domainChar(X) when X >= $0, X =< $9 -> true;
is_domainChar($.) -> true;
is_domainChar($_) -> true;
is_domainChar($-) -> true;
is_domainChar($@) -> true;      %% because we accept emails!
is_domainChar(X) -> false.

%% ----------------------------------------------------------------------------------------
%% Called by HTTP Post Handler - set the number of Agents 
%% ----------------------------------------------------------------------------------------

setAgents(CurMax)->
	AgentOpts=["1","2","5","10","20","50","100"],

	AgentRadioButtons=
		lists:map(fun(Z)->
		     	   	{input,[{type,radio},
			    		{name,numagents},
					 checked(CurMax,Z),
			    	        {value,Z}],
				       [Z,{br,[]}]}
			end,AgentOpts),

	FLs=[
		td_class(prompt,"Select the number of Agents"),
		td_class(select,[AgentRadioButtons])
	],

	Rows=lists:map(fun(Z)->{tr,[],Z} end,[FLs]),
	Table={table,[{class,configure}],[Rows]},

	Header={h1,[{class,formheader}],["Set Maximum Number of agents"]},
	Comment={p,[{class,comment}],["Additional comments here"]},
	Submit={input,[{type,"submit"},{value,"Set"}],[]},
	Form={form,[{method,"POST"},{action,"setAgents"}],
              [Header,Comment,Table,Submit]},
	{'div',[{id,setAgents}],Form}.

checked(N,N)->
	{checked,true};

checked(_,_)->
	[].

syslogger()->

	Header={h1,[{class,formheader}],["Set Module Logging Levels"]},
	Comment={p,[{class,comment}],["Additional comments here"]},

	Only=syslogger:getOnly(),

	%% note is Only is anything but [], we need to
	%% disable every radio button set except the
	%% one that corresponds to the Module for which
	%% Only is set

	?info({only,Only}),
	CurrentSyslog=lists:sort(syslogger:list()),

	%% Only is not [], then filter the CurrentSyslog list
	%% to remove all items EXCEPT that for the module

	DisplayModules=
	case Only of
	[]->
		CurrentSyslog;

	Module->
		lists:filter(fun(Z)->isModule(Module,Z) end,CurrentSyslog)

	end,

	Rows=lists:map(fun(Z)->{tr,[],r(Z,Only)} end,DisplayModules),
	TBody={tbody,[],Rows},
	THead={thead,[],[{tr,[],[{th,[],"Module"},{th,[],"Trace Level"},{th,[],"Filter"}]}]},

	Form=[Header,Comment,{table,[{id,"syslogtable"}],[THead,TBody]}],

	{'div',[{id,sylogger}],Form}.

isModule(Module,{Module,_})->
	true;

isModule(_,_)->
	false.

r({Module,Level},Only)->
	M=atom_to_list(Module),
	RadioButtons=lists:map(fun(Z)->genSyslogRadioButtons({M,Level,Only},Z) end,[0,1,2,3]),

	OnlyButton={input,[{type,'CHECKBOX'},
			   {class,'SYSLOG'},
			   {name,?TRACE_ONLY_TAG},
			   {value,Module},
			   checked(Module,Only)],"This Module Only"},
	[{td,[],M},{td,[],RadioButtons},{td,[],OnlyButton}].

tr(3)->
	"CRITICAL";
tr(2)->
	"WARN";
tr(1)->
	"INFO";
tr(0)->
	"DEBUG".

genSyslogRadioButtons({Module,CurLevel,Only},OptLevel)->
	{input,[{type,'RADIO'},
    		{class,'SYSLOG'},
		{name,Module},
		checked(CurLevel,OptLevel),
		{value,OptLevel}],
		tr(OptLevel)}.

%% this is the main page render components

page(Content)->
	case (catch cmanager:status()) of
	{ok,{{manager,CM=#cm_state{}},{controller,{CC=#cc_state{},_AS,_AP}}}}->
		page(Content,#state{cm=CM,cc=CC,io_data=[]});
	Other->
		{status,500}
	end.

page(Content,State=#state{cm=#cm_state{cstate=CSTATE}})->
	?dbug({renderingPage,{state,State}}),
	Title={title,[],"SPIDER Management Interface"},
	Style={link,[{rel,"stylesheet"},{type,"text/css"},{href,?STYLE}]},

	Javascript={ssi,"/js/spider.js",[],[]},
	Header={head,[],[Javascript,Title,Style]},
	Dynamic={'div',[{id,dynamic}],dynamic(State)},
	Wrapper={'div',[{id,bodyWrap}],
		       [header(),
			{hr,[{class,clear}],[]},
			dynamic(State),
			{hr,[{class,clear}],[]},
			navblock(State),
			main(Content,State),
			footer()]},

	Body={body,[],Wrapper},
	{ehtml,[?DOCTYPE,{html,[],[Header,Body]}]}.

header()->
	Engine="Engine: "++atom_to_list(erlang:node()),
	{'div',[{id,header}],[{h1,[],"Erlang SPIDER Management Interface - "++Engine}]}.

navblock(State)->
	{'div',[{id,navblock}],[menu(State)]}.

menu(State)->
	SMenu=menuOptions(State),	%% state dependent options

	Menu=[?MENU_MAIN,?MENU_SYSLOG],

	FMenuEntries=lists:map(fun({Text,Action})->{li,[{class,"menuitem"}],{a,[{href,Action}],Text}} end,Menu),
	SMenuEntries=lists:map(fun({Text,Action})->{li,[{class,"smenuitem"}],{a,[{href,Action}],Text}} end,SMenu),

	{ul,[{id,"menulist"}],[FMenuEntries,SMenuEntries]}.

footer()->
	{'div',[{id,footer}],[{p,[],"&copy; Melbourne IT Ltd 2008 "}]}.

main(Content,State)->
	[{'div',[{id,main}],[Content]},{hr,[{class,clear}],[]}].

%% dynamic display of various state variables
%% implement via a list of funs which each take the #state{} as argument

dynamicFuns()->
	RunState=fun(Z)->dynamicOp(runstate,Z) end,
	DataSource=fun(Z)->dynamicOp(datasource,Z) end,
	Agents=fun(Z)->dynamicOp(agents,Z) end,
	Memory=fun(Z)->dynamicOp(memory,Z) end,
	Filter=fun(Z)->dynamicOp(filter,Z) end,
	Writer=fun(Z)->dynamicOp(writer,Z) end,
	[RunState,DataSource,Agents,Filter,Writer].

dynamic(State=#state{cm=#cm_state{cstate=CSTATE}})->
	DD=lists:map(fun(Z)->Z(State) end,dynamicFuns()),
	Ds=lists:map(fun({Id,PayLoad})->{'div',[{class,dynamic},{id,Id}],PayLoad} end,DD),
	{'div',[{id,dynamic_container}],Ds};

dynamic(_Other)->
	[].

%% dynamic fun implementation ...

href(Description,Url)->
	{a,[{href,Url}],[Description]}.

dynamicOp(Id,Arg)->
	{Id,i_dynamicOp(Id,Arg)}.

i_dynamicOp(runstate,#state{cm=#cm_state{cstate='RUNNING'}})->
	[{p,[],["RUNNING"]}];

i_dynamicOp(runstate,#state{cm=#cm_state{cstate=CSTATE}}) when is_atom(CSTATE)->
	{p,[],[atom_to_list(CSTATE)]};

i_dynamicOp(datasource,#state{cm=#cm_state{datasource=ready}})->
	[href({img,[{src,"/gfx/database.png"},{alt,"image"}]},"/engine/datastatus"),
	 {p,[],["ready"]}];

i_dynamicOp(datasource,#state{cm=#cm_state{datasource=empty}})->
	[{img,[{src,"/gfx/database-empty.png"},{alt,"image"}],[]},
	 {p,[],"empty"}];

i_dynamicOp(datasource,#state{cm=#cm_state{datasource=Other}})->
	[{img,[{src,"/gfx/database-undefined.png"},{alt,"image"}],[]},
	 {p,[],"undefined"}];

i_dynamicOp(filter,#state{cm=#cm_state{jobConf=#jobConf{filter=Filter}}})->
	case rlogger:status() of
	{ok,#rlogger{filestate=open,writes=W}}->
		[{img,[{src,"/gfx/file-open.png"},{alt,"image"}],[]},
		 {p,[],"open"}];

	{ok,#rlogger{filestate=closed,writes=W}} when W>0->
		[href({img,[{src,"/gfx/file-closed.png"},{alt,"image"}],[]},"/engine/output"),
		{p,[],"closed"}];

	{ok,#rlogger{filestate=closed}}->
		[{img,[{src,"/gfx/file-closed.png"},{alt,"image"}],[]},
		{p,[],"closed"}];

	{ok,#rlogger{}}->
		[{img,[{src,"/gfx/file-undefined.png"},{alt,"image"}],[]},
		{p,[],"no file"}]
	end;

i_dynamicOp(writer,_)->
	{{requests,R},_}=datasource:getStats(),
	case rlogger:status() of
	{ok,#rlogger{filestate=FS,writes=W,errors=E,nulls=N}} when is_integer(W),
								   is_integer(E),
								   is_integer(N)->
		[
		 {'div',[{id,gets}],[{p,[],"gets: "++integer_to_list(R)}]},
		 {'div',[{id,filewrites}],[{p,[],"writes: "++integer_to_list(W)}]},
		 {'div',[{id,filenulls}],[{p,[],"nulls: "++integer_to_list(N)}]},
		 {'div',[{id,fileerrors}],[{p,[],"errors: "++integer_to_list(E)}]}];

	Other->
		[]
	end;

i_dynamicOp(agents,#state{cm=#cm_state{controllerConf=#controllerConf{numagents=N}},
		        cc=#cc_state{curagents=CA}}) when is_integer(CA)->
	IMG=href({img,[{src,"/gfx/agents.png"},{alt,"image"}],[]},"/engine/setAgents?num="++integer_to_list(N)),
	P=io_lib:format("~p(~p)",[CA,N]),
	%% C=[IMG,{p,[{id,"agents"}],[P]}];
	C=[IMG,{p,[{id,"_agents"}],[P]}];

i_dynamicOp(memory,_)->
	{pre,[],[integer_to_list(erlang:memory(total))]};

i_dynamicOp(Other,State)->
	?warn({unhandledDynamicOp,Other,State}),
	[].



%%---------------------------------------------------------------------------
%% menu options dependent on the STATE
%%---------------------------------------------------------------------------

menuOptions(State=#state{cm=#cm_state{cstate=CSTATE,
				      datasource=DS,
				      jobConf=JC}})->
	menuOptions(CSTATE,DS,JC).

menuOptions('IDLE',undefined,JC)->
	[?MENU_CONF,?MENU_LOADFILE,?MENU_LOADLIST];

menuOptions('IDLE',empty,JC)->
	[?MENU_CONF,?MENU_LOADFILE,?MENU_LOADLIST];

menuOptions('IDLE',ready,JC=#jobConf{moduleid=M,functionid=F}) when M=/=undefined,F=/=undefined->
	[?MENU_START,?MENU_CONF,?MENU_FILTER,?MENU_PURGE];

menuOptions('IDLE',ready,JC)->
	[?MENU_START,?MENU_CONF,?MENU_PURGE];

menuOptions('INIT',_,JC)->
	[?MENU_STOP];

menuOptions('READY',_,JC)->
	[?MENU_STOP];

menuOptions('RUNNING',_,JC)->
	[?MENU_SUSPEND,?MENU_STOP];

menuOptions('WAIT_STOP',_,JC)->
	[];

menuOptions('SUSPENDING',_,JC)->
	[];

menuOptions('SUSPENDED',_,JC)->
	[?MENU_STOP,?MENU_RESUME];

menuOptions('COMPLETED',_,JC)->
	[?MENU_STOP];

menuOptions(Other,_,_)->
	?warn({unhandledMenuOption,Other}),
	[].

%%---------------------------------------------------------------------------
	

%% general purpose rendering functions


html(ok)->
	html({ok,[]});

html(I={ok,Info})->
	{'div',[{id,cmdok}],[{pre,[],io_lib:print(I)}]};

html(E={error,Reason})-> 
	{'div',[{id,cmderror}],[{pre,[],io_lib:print(E)}]};

html(E={'EXIT',Reason})-> 
	html({error,E});

html(Other)->
	{'div',[{id,cmdformat}],[{pre,[],io_lib:print(Other)}]}.

%% File Upload Support ----------

uploadFile()->
	?info({uploadFile}),
	Header={h1,[{class,formheader}],["Select File to Upload"]},
	Comment={p,[{class,comment}],["Additional comments here"]},
	Submit={input,[{type,"submit"},{value,"Upload"}],[]},
	FileName={input,[{type,"file"},
			 {width,"50"},
			 {name,"fname"}],[]},

	Form={form,[{method,"POST"},
		    {action,"upload"},
		    {enctype,"multipart/form-data"}],[FileName,{hr,[],[]},Submit]},

	{'div',[{id,fileupload}],[Header,Comment,Form]}.


importFile()->
	Submit={td,[],{input,[{type,"submit"},{value,"Load"}],[]}},
	FilePrompt={td,[],"File Name"},
	FileInput={td,[],{input,[{name,"filename"},{type,"text"}],[]}},

	ErlRadio={input,[{type,"radio"},{name,"filetype"},{value,"erl"}],["erl"]},
	CSVRadio={input,[{type,"radio"},{name,"filetype"},{value,"csv"},{checked,"true"}],["csv"]},

	Radios={td,[],[ErlRadio,CSVRadio]},

	Rows={tr,[{class,importSpec}],[FilePrompt,FileInput,Radios,Submit]},
	Table={table,[],Rows},

	Form={form,[{method,"POST"},{action,"importdatafile"}],
              [Table]},
	{'div',[{id,importdatafile}],Form}.

importList()->
	Header={h1,[{class,formheader}],["Enter List of Domains"]},
	Comment={p,[{class,comment}],["Additional comments here"]},
	Submit={input,[{type,"submit"},{value,"Load"}],[]},
	Reset={input,[{type,"reset"},{value,"Clear"}],[]},
	EntryBlock={textarea,[{name,"domainlist"},
			   {type,"textarea"},
			   {cols,50},{rows,20}],[]},
	Form={form,[{method,"POST"},{action,"importdatalist"}],
		    [EntryBlock,{hr,[],[]},Submit,Reset]},
	{'div',[{id,importdatalist}],[Header,Comment,Form]}.


%% parameter extraction utility with defaults ....

getParam(Param,QS)->
	getParam(Param,QS,[]).

getParam(Param,QS,Default)->
	getParam(Param,QS,Default,lists:keysearch(Param,1,QS)).

getParam(Param,QS,_,{value,{Param,Value}})->
	Value;

getParam(Param,QS,Default,_)->
	?warn({default,{Param,Default}}),
	Default.


%% configuration setting 

td_class(Content)->
	{td,[],Content}.

td_class(Class,Content)->
	{td,[{class,Class}],Content}.

textarea(Name,Cols,Rows,Content)->
	{textarea,[{name,Name},{cols,Cols},{rows,Rows}],Content}.

setFilter(State=#state{cm=#cm_state{jobConf=JC=#jobConf{}}})->

	%% this (filter) should be a separate menu after the main configuration
	%% TBD, if the filter is set, pre check the radio button

	%% the currently set filter is in #jobConf.filter
	%% typical values are: xml,raw

	CurrentFilter=JC#jobConf.filter,

	?info({currentFilter,CurrentFilter}),

	Filters=JC#jobConf.filters,

	FilterRadioButtons=
		lists:map(fun(#filter_opt{key=K,
					  type=#file_type{desc=TD},
					  desc=D})->
				  {input,[{type,radio},
				  	  {name,"filter"},
				  	  {value,K},
				  	  checked(K,CurrentFilter)],
                		  	  [D++" "++TD,{br,[],[]}]} end, Filters),

	Header={h1,[{class,formheader}],["Output Filter Configuration Form"]},
	Comment={p,[{class,comment}],["Set the output filter here"]},
	Submit={input,[{type,"submit"},{value,"Set Filter"}],[]},
	Reset={input,[{type,"reset"},{value,"Clear"}],[]},

	FLs=[
		td_class(prompt,"Select the type of output Filter"),
		td_class(select,[FilterRadioButtons])
	],

	Rows=lists:map(fun(Z)->{tr,[],Z} end,[FLs]),

	Table={table,[{class,configure}],[Rows]},

	Form={form,[{method,"POST"},{action,"setFilter"}],
		    [Table,{hr,[],[]},Submit,Reset]},

	{'div',[{id,configure}],[Header,Comment,Form]}.



configure(State=#state{cm=#cm_state{jobConf=JC}}) when is_record(JC,jobConf)->

	?dbug({configure,{jobConf,JC}}),

	%% the current analysis is defined by #jobConf.moduleid and
	%% #jobConf.functionid

	CurrentKey=JC#jobConf.analysis_key,

	AnalysisRadioButtons=
		lists:map(fun(#analysis_opt{key=Key,desc=Description})->
			{input,[{type,radio},
				{name,analysis},
				checked(Key,CurrentKey),
				{value,Key}],
				[Description,{br,[],[]}]} end,
			?ANALYSIS),


	Header={h1,[{class,formheader}],["Configuration Form"]},
	Comment={p,[{class,comment}],["Set the Job Configuration here"]},
	Submit={input,[{type,"submit"},{value,"Configure"}],[]},
	Reset={input,[{type,"reset"},{value,"Clear"}],[]},


	Analysis=[
		td_class(prompt,"Select the type of Analysis"),
		td_class(select,[AnalysisRadioButtons])
	],

	Rows=lists:map(fun(Z)->{tr,[],Z} end,[Analysis]),

	Table={table,[{class,configure}],[Rows]},

	Form={form,[{method,"POST"},{action,"configure"}],
		    [Table,{hr,[],[]},Submit,Reset]},

	{'div',[{id,configure}],[Header,Comment,Form]};

configure(State=#state{cm=#cm_state{jobConf=JC}})->
	{'div',[{id,configure}],[]}.


%%-------------------------------------------------------------------------------
%% file upload handling .....
%%-------------------------------------------------------------------------------


% multipart(A,State) ->
% 	Parse=yaws_api:parse_multipart_post(A),
% 	case Parse of
% 	{cont,Cont,Res} ->
% 		case addFileChunk(A,Res,State) of
% 		{done,Result} ->
% 			Result;
%                 {cont,NewState} ->
% 			{get_more,Cont,NewState}
% 		end;
% 
% 	{result,Res} ->
% 		case addFileChunk(A,Res,State#upload{last=true}) of
% 		{done,Result} ->
% 			Result;
% 		{cont,_} ->
% 			{error,upload}
% 		end
% 	end.

%%%%%%%%%%% ----------------END

err()->
	error.

multipart(A, State) ->
    Parse = yaws_api:parse_multipart_post(A),
    case Parse of
        {cont, Cont, Res} ->
            case addFileChunk(A, Res, State) of
                {done, Result} ->
                    Result;
                {cont, NewState} ->
                    {get_more, Cont, NewState}
            end;
        {result, Res} ->
            case addFileChunk(A, Res, State#upload{last=true}) of
                {done, Result} ->
                    Result;
                E={cont, _} ->
                   {error,E}
            end;
        E1={error, _Reason} ->
            E1
    end.



addFileChunk(A, [{part_body, Data}|Res], State) ->
    addFileChunk(A, [{body, Data}|Res], State);

addFileChunk(_A, [], State) when State#upload.last==true,
                                 State#upload.filename /= undefined,
                                 State#upload.fd /= undefined ->

    file:close(State#upload.fd),
    Res = {ok,{uploaded,State#upload.filename}},
    {done, Res};

addFileChunk(A, [], State) when State#upload.last==true ->
    {done, err()};

addFileChunk(_A, [], State) ->
    {cont, State};

addFileChunk(A, [{head, {_Name, Opts}}|Res], State ) ->
    case lists:keysearch("filename", 1, Opts) of
        {value, {_, Fname0}} ->
            Fname = yaws_api:sanitize_file_name(basename(Fname0)),

            %% we must not put the file in the
            %% docroot, it may execute uploade code if the
            %% file is a .yaws file !!!!!
            file:make_dir(?UDIR),
            case file:open([?UDIR, Fname] ,[write]) of
                {ok, Fd} ->
                    S2 = State#upload{filename = Fname,
                                      fd = Fd},
                    addFileChunk(A, Res, S2);
                Err ->
                    {done, Err}
            end;
        false ->
            addFileChunk(A,Res,State)
    end;

addFileChunk(A, [{body, Data}|Res], State)
  when State#upload.filename /= undefined ->
    case file:write(State#upload.fd, Data) of
        ok ->
            addFileChunk(A, Res, State);
        Err ->
            {done, Err}
    end.


basename(FilePath) ->
    case string:rchr(FilePath, $\\) of
        0 ->
            %% probably not a DOS name
            filename:basename(FilePath);
        N ->
            %% probably a DOS name, remove everything after last \
            basename(string:substr(FilePath, N+1))
    end.


%% ------------------------------------------------------
%% miscellaneous formatting functions -- rewrite this TBD
%% ------------------------------------------------------

format_record(Record,Name,Fields) ->
	case tuple_to_list(Record) of
	[Name|Rest] ->
		%% Name is the record name
		%% Rest is the list of record field values
		Z=lists:zip(Fields,Rest),
		{Name,Z};
	X ->
		{error,{badrecord,X}}
     end.

