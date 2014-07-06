-module(esn_logger).
-behaviour(gen_server).
-include("esn_kernel.hrl").


%%% SASL API
-export([	init/1,
		handle_call/3,
		handle_cast/2,
	 	handle_info/2,
		code_change/3,
		terminate/2]).


%%% USER API
-export([	start_link/0,
		stop/0,
		status/0,
		get_state/1,
		log_enable/1,
		sync_logevent/2,
		logevent/2]).


%%% DEFINITIONS

-define(DEFAULT_TIMEOUT, 	5000).
-define(START_OPTIONS,		[]).
-define(EVM_CHECK_INTERVAL,	5000).

-define(SERVERNAME,		esn_logger).
-define(SERVER_START,		{local,esn_logger}).

-define(DEFAULT_LOGDIR,		"/tmp").
-define(DEFAULT_MAXFILES,	2).
-define(DEFAULT_MAXFILESIZE,	1000000).


%%% RECORDS

-record(state, {
		dir,
		maxB,
		maxF,
		curB,
		curF,
		curFd,
		enabled}).

%%% INIT FUNCTIONS

%%%	dir 	->	the directory where the log files will be written
%%%	maxB	-> 	the maximum size of each log file
%%%	maxF	->	the maximum number of logfiles 
%%%
%%%	Log filenames will be constructed as follows:
%%%
%%%	dir/node@host.lognn	(nn if MAX_LOGFILES > 9)
%%%	
%%%	e.g.	/tmp/hostmgr@zeblka222.log01
%%%
%%%	Index filenames will be constructed as follows:
%%%
%%%	dir/node@host.index
%%%
%%%	e.g.	/tmp/hostmgr@zeblka222.index

start_link()->
	gen_server:start_link(	?SERVER_START,
				?SERVERNAME,
				[],
				?START_OPTIONS).

stop()->
	gen_server:call(?SERVERNAME,die).


get_state(logfile)->
	gen_server:call(?SERVERNAME,{get_state,logfile}).

log_enable(Mode)->
	gen_server:call(?SERVERNAME,{log_enable,Mode}).

get_env(Key,Default)->
	case application:get_env(esn_kernel,Key) of
	{ok,Value}->
		Value;
	_->
		Default
	end.
	

init(_) ->
	process_flag(trap_exit,true),

	Dir=get_env(logdir,?DEFAULT_LOGDIR),
	MaxF=get_env(logmaxfiles,?DEFAULT_MAXFILES),
	MaxB=get_env(logmaxfilesize,?DEFAULT_MAXFILESIZE),

	State=#state{dir=Dir,maxB=MaxB,maxF=MaxF,enabled=true},


	%%%	if Index file exists, it will contain the sequence # 
	%%%	of the current logfile to start writing to. If not
	%%%	exist, start with number 1 (of course). NOTE that when 
	%%%	esn_logger starts, it will ALWAYS start logging to a
	%%%	new file, even if the "previous" file had not reached
	%%%	maximum size

	error_logger:delete_report_handler(error_logger),
	case lists:member(esn_report_h,
			  gen_event:which_handlers(error_logger)) of
        false->
                %% OK, we can go and install it
                error_logger:add_report_handler(esn_report_h);
        _->
                %% oops, already installed, maybe we should log this as an
                %% error ??
                ok
        end,

	First=
		case readIndexFile(State#state.dir) of
		{ok,LastWritten}->
			inc(LastWritten,MaxF);
		_->
			1
		end,

	case catch file_open(First,State) of
	{ok,Fd}->
		Len=openLog(Fd),
		InitMsg=esn_event_format:format("*PRG*",{?SERVERNAME,{started,
									{pid,self()},
									{fileNo,First}}}),
		io:format(Fd,InitMsg,[]),
		LenInitMsg=length(lists:flatten(InitMsg)),
		NextState=State#state{curF=First,curFd=Fd,curB=LenInitMsg+Len},
		{ok,NextState};

	Error->
		{stop,Error}
	end.

code_change(_OldVsn,State,_Extra)->
	{ok,State}.


status()->
	gen_server:call(?SERVERNAME,status).

logevent(Event,EventType)->
	gen_server:cast(?SERVERNAME,{log,Event,EventType}).

sync_logevent(Event,EventType)->
	gen_server:call(?SERVERNAME,{log,Event,EventType}).


%%% INTERNAL FUNCTIONS

readIndexFile(Dir)->
	IndexFileName=Dir++"/"++atom_to_list(node())++"."++?INDEX_SUFFIX,
	case file:consult(IndexFileName) of
	{ok,[I]} when is_integer(I)->
		{ok,I};
	Error->	
		Error
	end.

writeIndexFile(Index,State)->
	IndexFileName=State#state.dir++"/"++atom_to_list(node())++"."++?INDEX_SUFFIX,
	case file:open(IndexFileName,write) of
	{ok,Fd}->
		io:format(Fd,"~p.~n",[Index]),
		file:close(Fd);

	{error,Reason}->
		exit({esn_logger,{writeIndexFile,Reason}})
	end.

file_open(FileNo,State)->
	LogFileName=log_file_name(State#state.dir,FileNo),
	case file:open(LogFileName,write) of
	{ok,Fd}->
		writeIndexFile(FileNo,State),
		{ok,Fd};

	{error,Reason}->
		exit({esn_logger,{file_open,LogFileName,Reason}})
	end.

inc(N,Max)->
	if N < Max->
		N+1;
	true->
		1
end.

zeropad(Num,Size)->
	if length(Num) < Size->
		zeropad("0"++Num,Size);
	true->
		Num
	end.

log_file_name(Dir,FileNo)->
	Suffix=zeropad(integer_to_list(FileNo),length(integer_to_list(?MAX_LOGFILES))),
	Dir ++ "/" ++ atom_to_list(node()) ++ ".log" ++ Suffix.

openLog(Fd)->
	OpenMsg="**LOG FILE OPENED",
	io:format(Fd,"~s~n",[OpenMsg]),
	length(OpenMsg)+1.

closeLog(Fd,Reason)->
	io:format(Fd,"**LOG FILE CLOSED [~p]~n",[Reason]),
	file:close(Fd).

handle_call(die,_,State)->
	{stop,normal,ok,State};

handle_call(status,_,State)->
	{reply,State,State};


handle_call({get_state,logfile},_,State)->
	Reply=[	{currentfile,log_file_name(State#state.dir,State#state.curF)},
		{currentsize,State#state.curB},
		{max_size,State#state.maxB},
		{max_files,State#state.maxF},
		{logging_enabled,State#state.enabled}],
	{reply,{?SERVERNAME,node(),{logfile,Reply}},State};

handle_call({log_enable,true},_,State) when State#state.enabled==false->
	{reply,{?SERVERNAME,node(),log_enable,true},State#state{enabled=true}};

handle_call({log_enable,false},_,State) when State#state.enabled==true->
	{reply,{?SERVERNAME,node(),log_enable,false},State#state{enabled=false}};

handle_call({log_enable,_Mode},_,State)->
	{reply,{?SERVERNAME,node(),log_enable,State#state.enabled},State};

handle_call({log,Event,EventType},_,State) when State#state.enabled==true->
	S=esn_event_format:formatEvent(Event,EventType),
	NextState=writeReport(S,State),
	{reply,ok,NextState};

handle_call(_Msg,{_Source,_Tag},State) ->
	{noreply,State}.

handle_cast({log,Event,EventType},State) when State#state.enabled==true->
	S=esn_event_format:formatEvent(Event,EventType),
	NextState=writeReport(S,State),
	{noreply,NextState};

handle_cast(_Msg,State) ->
	{noreply,State}.

handle_info(_Msg,State) ->
	{noreply,State}.

terminate(Reason,State) ->
	ExitMsg=esn_event_format:format("*PRG*",{?SERVERNAME,{terminating,Reason}}),
	io:format(State#state.curFd,ExitMsg,[]),
	closeLog(State#state.curFd,{terminated,Reason}),
  	ok.

writeReport(S,State)->
	CurB=State#state.curB,
	CurF=State#state.curF,
	CurFd=State#state.curFd,
	Size=length(lists:flatten(S)),
	NewState=
		if CurB+Size < State#state.maxB-> 
			State;
		true ->
			closeLog(CurFd,'reached maximum size'),
			NewF=inc(CurF,State#state.maxF),
			{ok,NewFd}=file_open(NewF,State),
			Len=openLog(NewFd),
			State#state{curFd=NewFd,curF=NewF,curB=Len}
		end,
	io:format(NewState#state.curFd,S,[]),
	NewState#state{curB=NewState#state.curB+Size}.



