-module(rlogger).
-behaviour(gen_server).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-include("agent.hrl").
-include("rlogger.hrl").
-include_lib("kernel/include/file.hrl").


%%% SASL API
-export([	init/1,
		handle_call/3,
		handle_cast/2,
	 	handle_info/2,
		terminate/2]).


%%% USER API
-export([	start/0,
		start_link/0,
		stop/0,
		status/0,
		addFilter/1,
		deleteFilter/0,
		getWriteStats/0,
		getFile/0,
		reset/2,	%% closes current file (if any), and
		reset/1,	%% closes current file (if any), and
				%% opens new file
		close/0,	%% closes the current file (if open)
		write/1]).


%%% DEFINITIONS

-define(DEFAULT_TIMEOUT, 	5000).
-define(START_OPTIONS,		[]).
-define(EVM_CHECK_INTERVAL,	5000).

-define(SERVERNAME,		rlogger).
-define(SERVERCALL,		{local,rlogger}).

-define(DEFAULT_LOGDIR,		"/tmp").


%%% RECORDS

-record(state, {
		dir,		%% directory name
		fd,		%% file descriptor pid
		filename,	%% filename
		filter,		%% filter to be applied to all output
		enabled,
		writes,		%% number of writes
		nulls,		%% null filter responses
		errors		%% number of write errors
}).

%%% INIT FUNCTIONS

start_link()->
	start().

start()->
	gen_server:start_link(	?SERVERCALL,
				?SERVERNAME,
				[],
				?START_OPTIONS).

stop()->
	gen_server:call(?SERVERNAME,die).

openFile(Dir,F)->
	FileName=Dir++"/"++F,
	case file:open(FileName,write) of
        {ok,Fd}->
                {ok,Fd};

        E={error,_Reason}->
                ?warn({rlogger,E,{file,FileName}}),
                E
        end.

close()->
	gen_server:call(?SERVERNAME,close).


init(_) ->
	process_flag(trap_exit,true),

	Dir=
	case cconfigdb:lookup('DG_AGENT_DEFAULT_LOGDIR') of
	undefined->
		?DEFAULT_LOGDIR;
	{ok,D}->
		D
	end,
	{ok,#state{dir=Dir,enabled=true}}.


%% add a filter to post_process an #agentResult record

addFilter(MF={_M,_F})->
	gen_server:call(?SERVERNAME,{addFilter,MF});

addFilter(Fun) when is_function(Fun)->
	gen_server:call(?SERVERNAME,{addFilter,Fun});

addFilter(raw)->
	gen_server:call(?SERVERNAME,{addFilter,raw});

addFilter(xml)->
	gen_server:call(?SERVERNAME,{addFilter,xml});

addFilter(Filter)->
        {error,{badarg,Filter}}.

deleteFilter()->
	gen_server:call(?SERVERNAME,deleteFilter).

getFile()->
	gen_server:call(?SERVERNAME,getFile).

reset(Dir,FileName)->
	gen_server:call(?SERVERNAME,{reset,Dir,FileName}).

reset(FileName)->
	gen_server:call(?SERVERNAME,{reset,FileName}).

status()->
	gen_server:call(?SERVERNAME,status).

write(Msg)->
	%% send the message to the logger event handler
	gen_server:cast(?SERVERNAME,{write,Msg}).

getWriteStats()->
	gen_server:call(?SERVERNAME,getWriteStats).


%%% INTERNAL FUNCTIONS

handle_call(die,_,State)->
	{stop,normal,ok,State};

handle_call({addFilter,F},_,State)->
	?info({addFilter,F}),
        {reply,ok,State#state{filter=F}};

handle_call(deleteFilter,_,State)->
	?info(removeFilter),
        {reply,ok,State#state{filter=undefined}};

handle_call(close,_,State=#state{fd=Fd,dir=Dir,filename=F}) when is_pid(Fd)->
	?info({closingFile,{dir,Dir},{file,F},{pid,Fd}}),
	file:close(Fd),
	{reply,ok,State#state{fd=undefined}};

handle_call(getFile,_,State=#state{fd=Fd,
				   dir=Dir,
			           filename=F}) when is_pid(Fd)->

	{reply,{ok,Dir++"/"++F},State};

handle_call({reset,Dir,FileName},_,State=#state{fd=Fd,
					    dir=Dir,
					    filename=F}) when is_pid(Fd)->

	%% close existing log file, open new one

	?info({reset,{file,FileName},{dir,Dir}}),

	case openFile(Dir,FileName) of
	{ok,NewFd}->
		file:close(Fd),
		?info({fileClosed,F}),
		?info({fileOpen,FileName}),
		{reply,ok,State#state{	fd=NewFd,
					filename=FileName,
					dir=Dir,
					writes=0,
					nulls=0,
					errors=0}};

	E={error,Reason}->
		?warn({fileOpen,{file,FileName},E}),
		{reply,E,State}
	end;

handle_call({reset,FileName},_,State=#state{fd=Fd,
					    dir=Dir,
					    filename=F}) when is_pid(Fd)->

	%% close existing log file, open new one

	?info({reset,{file,FileName}}),

	case openFile(Dir,FileName) of
	{ok,NewFd}->
		file:close(Fd),
		?info({fileClosed,F}),
		?info({fileOpen,FileName}),
		{reply,ok,State#state{	fd=NewFd,
					filename=FileName,
					dir=Dir,
					writes=0,
					nulls=0,
					errors=0}};

	E={error,_Reason}->
		?warn({fileOpen,{file,FileName},E}),
		{reply,E,State}
	end;

handle_call({reset,FileName},_,State=#state{dir=Dir})->

	%% open new logfile

	?info({reset,{file,FileName}}),

	case openFile(Dir,FileName) of
	{ok,Fd}->
		?info({fileOpen,FileName}),
		{reply,ok,State#state{	fd=Fd,
					filename=FileName,
					writes=0,
					nulls=0,
					errors=0}};

	E={error,_Reason}->
		?warn({fileOpen,{file,FileName},E}),
		{reply,E,State}
	end;

handle_call({reset,Dir,FileName},_,State=#state{})->

	%% open new logfile

	?info({reset,{file,FileName},{dir,Dir}}),

	case openFile(Dir,FileName) of
	{ok,Fd}->
		?info({fileOpen,FileName}),
		{reply,ok,State#state{	fd=Fd,
					filename=FileName,
					dir=Dir,
					writes=0,
					nulls=0,
					errors=0}};

	E={error,_Reason}->
		?warn({fileOpen,{file,FileName},E}),
		{reply,E,State}
	end;


handle_call(status,_,State=#state{fd=Fd,
			  	  filename=undefined,
				  dir=Dir,
				  writes=W,
				  nulls=N,
				  errors=E})->

	S=#rlogger{dir=Dir},

	{reply,{ok,S},State};
			

handle_call(status,_,State=#state{fd=Fd,
			  	  filename=FileName,
				  dir=Dir,
				  writes=W,
				  nulls=N,
				  errors=E})->

	Fstate=
	case is_pid(Fd) of
	true->
		open;
	_->
		closed
	end,

	Path=Dir++"/"++FileName,

	Info=
	case (catch file:read_file_info(Path)) of
	{ok,F=#file_info{}}->
		F;
	_->
		undefined
	end,

	S=#rlogger{	fd=Fd,
			dir=Dir,
			filename=FileName,
			path=Path,
			filestate=Fstate,
			writes=W,
			errors=E,
			nulls=N,
			info=Info},

	{reply,{ok,S},State};

handle_call(getWriteStats,_,State=#state{filename=F,
					 writes=W,
					 nulls=N,
					 errors=E})->
	{reply,{writes,W},State};



handle_call(Msg,{_Source,_Tag},State)->
	?warn({unhandled,Msg}),
	{reply,ok,State}.

%% this is where we actually transform and output the transformed message
%% to the logger file based on a tag in the message, we decide what transformer 
%% This should be architected so that all {write,Msg} messages first call
%% the transformer (e.g. this applies to all message payloads) which then
%% passes the result to the actual output functions


handle_cast({write,Msg},State=#state{fd=Fd,
				     filter=Filter,
				     writes=W,
				     nulls=N,
				     errors=E}) 
	when is_pid(Fd)->

	?dbug({applyFilter,{msg,Msg},{filter,Filter}}),

	case applyFilter(Msg,Filter) of
	{filter,null}->
		{noreply,State#state{nulls=N+1}};

	{filter,Result}->
		case handle_write(Fd,Result) of
		ok->
			{noreply,State#state{writes=W+1}};
		Other->
			{noreply,State#state{errors=E+1}}
		end;

	_Other->
		{noreply,State#state{errors=E+1}}
	end;


%% END TEST SECTION


handle_cast({write,Msg},State)->
	?warn({writeNoFd,Msg}),
	{noreply,State};

handle_cast(_Msg,State) ->
	{noreply,State}.

handle_info(_Msg,State) ->
	{noreply,State}.

terminate(_Reason,#state{fd=Fd}) ->
	file:close(Fd),
	ok.

handle_write(Fd,Result)->
	case (catch io:format(Fd,"~s~n",[Result])) of
	E={'EXIT',Reason}->
		?dbug({handle_write,Reason}),
		{error,E};
	_->
		ok
	end.

%%=============================================================================
%% apply the filter 
%%=============================================================================

applyFilter(Msg,undefined)->
	%% there is no filter defined
	?dbug({noFilter,Msg}),
	{nofilter,Msg};

applyFilter(Msg,MF={M,F})->
	case (catch apply(M,filter,[F,Msg])) of
	Z={filter,_Transformed}->
		?dbug({filterResult,Z}),
		Z;
	E={error,_Reason}->
		?warn({filterError,{mfa,MF,Msg},E}),
		E;
	Other->
		?warn({filterError,{mfa,MF,Msg},Other}),
		{error,Other}
	end;

applyFilter(Msg,Fun) when is_function(Fun)->
	case (catch Fun(Msg)) of
        Z={filter,_Transformed}->
                Z;
	E={error,_Reason}->
		?warn({filterError,Fun,E}),
		E;
        Other->
                ?warn({filterError,Fun,Other}),
                {error,Other}
        end;

applyFilter(Msg,raw)->
	Z=io_lib:format("~p",[Msg]),
	{filter,Z};

applyFilter(#agentResult{payload=PayLoad},xml)->
	applyFilter(PayLoad,xml);

applyFilter(Msg={{module,_Module},Data},xml)->
	%% TBD, need to atch this
	XML=serialise:request(Data),
	{filter,XML};


applyFilter(Msg,Filter)->
	?warn({unhandledFilter,{msg,Msg},{filter,Filter}}),
	{filter,null}.
