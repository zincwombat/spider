-module(filesource).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").
-behaviour(gen_server).

%% This module implements the datasource for the agents to query 

%% CONFIGDB Parameters --------------------------------------------------------


-export([       init/1,
                handle_call/3,
                handle_cast/2,
                handle_info/2,
                terminate/2
                ]).

-export([       start/0,
		start_link/0,
                start/1,
		load/1,
		close/0,
                status/0,
		fetch/0,
                stop/0]).

-define(START_OPTIONS,          []).
-define(SERVERNAME,             ?MODULE).
-define(SERVERACCESS,           {local,?SERVERNAME}).
-define(SERVERCALL,             ?SERVERNAME).


-include_lib("kernel/include/file.hrl").

-sysloglevel(?TRACE_WARN).

-define(CHUNKSIZE,4096).
-record(state,{filehandle,filename,filesize,offset,opaque}).

start_link()->
	start().

start()->
        start([]).

start(Arg)->
        gen_server:start_link(  ?SERVERACCESS,
                                ?SERVERNAME,
                                [Arg],
                                ?START_OPTIONS).

stop()->
        gen_server:call(?SERVERCALL,die).

init(_)->
        process_flag(trap_exit,true),
        ?info({started,self()}),
        {ok,#state{}}.

status()->
	gen_server:call(?SERVERCALL,status).

close()->
	gen_server:call(?SERVERCALL,close).

fetch()->
	gen_server:call(?SERVERCALL,fetch).

load(FileName)->
	gen_server:call(?SERVERCALL,{load,FileName}).

read(Offset,_File,_Length,Size) when Offset > Size->
	done;

read(Offset,File,Length,Size)->
	{ok, Data} = file:pread(File, Offset, Length),
	SData=binary_to_list(Data),
	read(Offset+Length,File,Length,Size).

%% callbacks


handle_call(die,_,State) ->
        {stop,normal,State};

handle_call({load,File},_,State=#state{filehandle=undefined})->
	handle_load_file(File,State);

handle_call({load,File},_,State=#state{filehandle=FH}) when is_pid(FH)->
	?info(closingFile),
	file:close(FH),
	handle_load_file(File,State#state{filehandle=undefined});

handle_call({load,File},_,State)->
	 ?info({wrong_state_load,File}),
	{reply,{error,{wrongstate,State}},State};

handle_call(fetch,_,State=#state{filehandle=FH,offset=Offset,filesize=Size}) when is_pid(FH)->
	?dbug({fetch,{offset,Offset}}),
	ChunkDelta=seek(FH,Offset+?CHUNKSIZE)-Offset,
	?dbug({chunkDelta,ChunkDelta}),
        case file:pread(FH,Offset,ChunkDelta) of
	{ok,Data}->
		SData=parseData(Data),
		?dbug({parsed,SData}),
		{reply,{ok,SData},State#state{offset=Offset+ChunkDelta}};
	Other->
		{reply,Other,State}
	end;

handle_call(fetch,_,State=#state{filehandle=undefined})->
	{reply,{error,nofile},State};

handle_call(close,_,State=#state{filehandle=FH}) when is_pid(FH)->
	Reply=file:close(FH),
	{reply,Reply,#state{}};

handle_call(status,_,State) ->
        {reply,{ok,State},State};

handle_call(Msg,_,State) ->
        ?dbug({unhandledCall,Msg,State}),
        {reply,ok,State}.

handle_cast(Msg,State)->
        ?dbug({unhandledCast,Msg,State}),
        {noreply,State}.

handle_info(Msg,State)->
        ?dbug({unhandledInfo,Msg,State}),
        {noreply,State}.

terminate(Reason,State)->
        ?info({stopping,Reason}),
        ok.

%% server side functions

parseData(BData) when is_binary(BData)->
	L=binary_to_list(BData),
	string:tokens(L,"\n").

%% returns position of next "\n" relative to Position

seek(FileHandle,Position)->
	R=file:pread(FileHandle,Position,1),
	seek(FileHandle,Position,R).

seek(FileHandle,Position,{ok,<<"\n">>})->
	Position;

seek(FileHandle,Position,{ok,<<A>>})->
	seek(FileHandle,Position+1);

seek(FileHandle,Position,eof)->
	Position.

%% load the file

handle_load_file(File,State)->
	?info({load,File}),
	case file:read_file_info(File) of
		{ok,FileInfo=#file_info{size=Size}} when Size>0->
			case file:open(File,[read,binary]) of
			{ok,FH}->
				{reply,ok,State#state{	filehandle=FH,
							filename=File,
							filesize=Size,
							offset=0}};
			Error->
				{reply,Error,State}
			end;
		Other->
			{reply,Other,State}
	end.
