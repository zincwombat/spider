-module(datasource).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").
-behaviour(gen_server).

%% This module implements the datasource for the agents to query 

%% CONFIGDB Parameters --------------------------------------------------------

%% {datasource,max_size}	-- maximum size allowed for the datasource ets


-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2
        ]).

-export([start/0,
	 start_link/0,
	 stop/0]).

-export([dget/0,		%% get next data, synchronous
	 dget_a/1,		%% async version
	 status/0,		%% get status information
	 nstatus/0,		%% normalised status
	 count/0,		%% return the number of data items in table
	 reset/0,		%% reset the data position
	 getStats/0,
	 purge/0		%% empty the table
	]).

-export([
	 import/1	
	]).

%% -include("monitor.hrl").
-include("agent.hrl").
-include("datasource.hrl").
-sysloglevel(?TRACE_WARN).

-define(START_OPTIONS,          []).
-define(SERVERNAME,             datasource).
-define(SERVERACCESS,           {local,?SERVERNAME}).
-define(SERVERCALL,             ?SERVERNAME).


start_link()->
	start().

start()->
        gen_server:start_link(  ?SERVERACCESS,
				?SERVERNAME,
				[],
                                ?START_OPTIONS).

stop()->
        gen_server:call(?SERVERCALL,die).


init(_)->
	%% create the ETS table to store the data

	DTab=ets:new(data_table,[set,protected]),

	process_flag(trap_exit,true),

	%% subscribe the configdb event handler

	configdb_evh:subscribe(),

	%% initialise the ds_state variables
	%% note that next will be set to '$end_of_table'

	MaxSize=
	case cconfigdb:lookup('DG_DATASOURCE_MAX_SIZE') of
	undefined->
		?DS_MAXDATA;
	{ok,Max}->
		Max
	end,

	?info({started,self()}),
		
	{ok,#ds_state{data_tab=DTab,
		   rq_count=0,
		   data_count=0,
		   max_size=MaxSize,
		   fetch_count=0,
		   next=ets:first(DTab)}}.

status()->
	gen_server:call(?SERVERCALL,status).

nstatus()->
	gen_server:call(?SERVERCALL,nstatus).

getStats()->
	gen_server:call(?SERVERCALL,getStats).

count()->
	gen_server:call(?SERVERCALL,count).

reset()->
	gen_server:call(?SERVERCALL,reset).

purge()->
	gen_server:call(?SERVERCALL,purge).

notify_listeners(Message)->
	datasource_evh:notify({?MODULE,Message}).

%% data loading functions
%% TBD, need to add an asynchronous variant of this


import(Import)->
	gen_server:call(?SERVERCALL,{import,Import},?DS_LONGTMOUT).


%% these functions are called by e.g. agents to get the next data to be 
%% analysed.

dget()->
	%% synchronous version
	gen_server:call(?SERVERCALL,dget,?DS_TMOUT).

dget_a(ReplyTo)->
	%% asynchronous version
	gen_server:cast(?SERVERCALL,{dget,ReplyTo}).

%% standard callback functions

%% synchronous CALL interface

handle_call(die,_,State)->
        {stop,normal,State};

handle_call(dget,From,State=#ds_state{next='$end_of_table',rq_count=RQ})->

	%% we have a request for data but there is no more data
	%% to serve up. We return a status message to the caller,
	%% and continue to update the request count

	?info({endoftable,{caller,From}}),
	
        {reply,{status,finished},State#ds_state{rq_count=RQ+1}};

handle_call(dget,_,State=#ds_state{data_tab=DTab,next=Next,rq_count=RQ,importSpec=Import})->

	%% we have a request for data, return the next data item
	%% and update the request count, get the next data item

	Next2=ets:next(DTab,Next),

	if Next2=='$end_of_table'->
		NextState=handle_end_of_table(State),
		{reply,{data,Next},NextState};
	true->
		%% just a normal request, and not the last one either

        	{reply,{data,Next},State#ds_state{rq_count=RQ+1,
					       next=Next2}}
	end;
		

handle_call(reset,_,State=#ds_state{data_tab=DTab})->

	%% reset the pointer to the start of the ets table
	%% and the request count

	notify_listeners(reset),
	?dbug(reset),

        {reply,ok,State#ds_state{next=ets:first(DTab),
			      rq_count=0,
			      fetch_count=0,
			      stop_tm=undefined,
			      start_tm=erlang:now()}};

handle_call(purge,_,State=#ds_state{data_tab=DTab,
				 data_count=DataCount})->

	%% empty the table of all the data

	?dbug(purge),

	ets:delete_all_objects(DTab),

	notify_listeners(purged),

        {reply,{ok,purged},State#ds_state{data_count=0,
			      next=ets:first(DTab),
			      importSpec=undefined,
			      start_tm=undefined,
			      stop_tm=undefined,
			      rq_count=0}};

handle_call(count,_,State=#ds_state{data_count=Count})->

	%% return the number of data items in the table
	?dbug(count),

        {reply,Count,State};

handle_call(getStats,_,State=#ds_state{rq_count=R,fetch_count=F})->

        {reply,{{requests,R},{fileFetches,F}},State};

handle_call(status,_,State=#ds_state{start_tm=StartTime,
				  stop_tm=undefined,
				  rq_count=RQ})->

	%% return the current data serving rate

	NowTime=erlang:now(),

	Rate=rate(StartTime,NowTime,RQ),

        {reply,{{rate,Rate},State},State};

handle_call(status,_,State=#ds_state{start_tm=_Start,
				  stop_tm=_Stop,
				  rq_count=_RQ})->

	%% return the overall average data serving rate 

	Rate=rate(State),
        {reply,{{rate,Rate},State},State};


handle_call(status,_,State)->
        {reply,State,State};

handle_call(nstatus,_,State)->
	%% normalised status
        {reply,State,State};
        %%{reply,objectMapper:map(State),State};


handle_call({import,Import=#import{}},_,State)->
	?dbug({import,Import}),
	handle_import(Import,State);

handle_call(Msg,_,State) ->
	%% unhandled message
	?info({unhandledCall,Msg}),
        {reply,ignored,State}.


%% calculate rates

rate(#ds_state{start_tm=Start,stop_tm=Stop,rq_count=RQ})->
	rate(Start,Stop,RQ).

rate({SMS,SS,SUS},{EMS,ES,EUS},Requests)->

	StartSec=(SMS*1000000)+SS+(SUS/1000000),
	EndSec=(EMS*1000000)+ES+(EUS/1000000),
	Elapsed=EndSec-StartSec,

	if Elapsed>0->
		%% protect against divide by 0 errors
		{{elapsed,Elapsed},
		 {requests,Requests},
 		 {rate_per_sec,Requests/Elapsed}};
	true->
		undefined
	end;

rate(_,_,_)->
	undefined.

%% the asynchronous interface uses the following functions to reply to the
%% caller.

reply(Reply,ReplyTo) when pid(ReplyTo)->
	ReplyTo ! Reply;

reply(Reply,ReplyTo) when is_function(ReplyTo)->
	catch ReplyTo(Reply);

reply(_Reply,ReplyTo)->
	?warn({badReplyTo,ReplyTo}),
	ok.

	
%% asynchronous CAST interface

handle_cast({dget,ReplyTo},State=#ds_state{next='$end_of_table',rq_count=RQ})->

	%% we have a request for data but there is no more data
	%% to serve up. We return a status message to the caller,
	%% and continue to update the request count

	reply({status,finished},ReplyTo),
	
        {noreply,State#ds_state{rq_count=RQ+1}};

handle_cast({dget,ReplyTo},State=#ds_state{data_tab=DTab,
			       		next=Next,
					start_tm=StartTime,
			       		rq_count=RQ})->

	%% we have a request for data, return the next data item
	%% and update the request count, get the next data item

	%% we have a request for data, return the next data item
	%% and update the request count, get the next data item

	Next2=ets:next(DTab,Next),

	if Next2=='$end_of_table'->
		NextState=handle_end_of_table(State),
		reply({data,Next},ReplyTo),
        	{noreply,NextState};

	true->
		%% just a normal request, and not the last one either

		reply({data,Next},ReplyTo),
        	{noreply,State#ds_state{rq_count=RQ+1,next=Next2}}
	end;


handle_cast(Msg,State)->
	%% unhandled message
	?info({unhandledCast,Msg}),
        {noreply,State}.

handle_info(Msg={event,{cconfigdb,Config}},State)->
	?info({handleInfo,Msg}),
	NewState=handle_config(Config,State),
        {noreply,NewState};

handle_info(Msg,State)->
	?info({unhandledInfo,Msg}),
        {noreply,State}.

terminate(Reason,_State) ->
	?critical({stopping,Reason}),
        ok.

%% CONFIGURATION CHANGE HANDLING -----------------------------------------------


handle_config({changed,'DG_DATASOURCE_MAX_SIZE',{{old,_Old},{new,New}}},
	      State=#ds_state{max_size=MaxSize})->
	%% max_size
	?info({configChange,max_size,{from,MaxSize},{to,New}}),
	State#ds_state{max_size=New};

handle_config(Config,State)->
	?warn({unhandledConfig,Config}),
	State.

%% utility functions

handle_import(Import=#import{	type=file,format="erl",value=Filename},
				State=#ds_state{data_tab=DTab,
						data_count=Count,
						max_size=Max})->
	%% assume file is a valid erlang term
	ST=calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	case file:consult(Filename) of
	{ok,[Data]}->
		%% we need to add each item in Data to the
		%% ets table, and update the ds_state variables
		%% accordingly

		%% insert the new items into the table 

		case insertData(Data,DTab,Max) of
		I={ok,Info}->
			NewCount=ets:info(DTab,size),
			Added=NewCount-Count,
			notify_listeners({import,{source,{file,Filename}},
					 {size,NewCount},
					 {status,I}}),

			ET=calendar:datetime_to_gregorian_seconds(calendar:local_time()),

			Reply={ok,{Info,{added,Added},
					{memory,ets:info(DTab,memory)},
					{elapsed,ET-ST}}},

			{reply,Reply,State#ds_state{data_count=NewCount,
				    importSpec=Import,
				    next=ets:first(DTab)}};
			

		Other->
			?warn({fileImportError,{file,Filename},Other}),
			{reply,Other,State}
		end;


	E={error,Reason}->
		notify_listeners({import,E}),
		?warn({fileImportError,{file,Filename},E}),
		{reply,{error,{Reason,{file,Filename}}},State}
	end;

handle_import(Import=#import{	type=file,format="csv",value=Filename},
				State=#ds_state{data_tab=DTab,
						data_count=Count,
						fetch_count=FetchCount,
						max_size=Max})->

	ST=calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	?info({load,{file,Filename}}),
	case (catch filesource:load(Filename)) of
	ok->
		case filesource:fetch() of
		{ok,List}->
			?dbug({fetch,List}),

			R=insertData(List,DTab,Max),

			case insertData(List,DTab,Max) of
			I={ok,Info}->
				NewCount=ets:info(DTab,size),
				Added=NewCount-Count,
				notify_listeners({import,{source,{file,Filename}},
                                 		  {size,NewCount},
                                 		  {status,I}}),

				ET=calendar:datetime_to_gregorian_seconds(calendar:local_time()),

				Reply={ok,{Info,{added,Added},
						{memory,ets:info(DTab,memory)},
						{elapsed,ET-ST}}},

				{reply,Reply,State#ds_state{data_count=NewCount,
                                       importSpec=Import,
				       fetch_count=FetchCount+1,
                                       next=ets:first(DTab)}};

			Other->
				?warn({listImportError,Other}),
				{reply,Other,State}
			end;

		Other->
			{reply,Other,state}
		end;


	E={error,Reason}->
		notify_listeners({import,E}),
		?warn({fileImportError,{file,Filename},E}),
		{reply,{error,{Reason,{file,Filename}}},State};

	Other2->
		notify_listeners({import,Other2}),
		?warn({fileImportError,{file,Filename},Other2}),
		{reply,{error,{Other2,{file,Filename}}},State}
		
	end;

handle_import(_Import=#import{	type=list,value=L},
				State=#ds_state{data_tab=DTab,
                                 		data_count=Count,
                                 		max_size=Max})->

        ST=calendar:datetime_to_gregorian_seconds(calendar:local_time()),
                                                                                
	case insertData(L,DTab,Max) of
	I={ok,Info}->
		NewCount=ets:info(DTab,size),
		Added=NewCount-Count,
        	notify_listeners({import,{source,list},
                                 {size,NewCount},
                                 {status,I}}),

		ET=calendar:datetime_to_gregorian_seconds(calendar:local_time()),

		Reply={ok,{Info,{added,Added},
				{memory,ets:info(DTab,memory)},
				{elapsed,ET-ST}}},

		{reply,Reply,State#ds_state{data_count=NewCount,
                                    	    importSpec=#import{type=list,
					    value={length,length(L)}},
					     next=ets:first(DTab)}};

	Other->
		?warn({listImportError,Other}),
		{reply,Other,State}
	end;

handle_import(_Import,State)->
	{reply,{error,badarg},State}.


handle_end_of_table(State=#ds_state{data_tab=DTab,
				    next=Next,
				    rq_count=RQ,
				    fetch_count=FetchCount,
				    max_size=Max,
				    importSpec=#import{type=file,format="csv"}})->
	?info({endOfTable,csv}),
	case filesource:fetch() of
	{ok,List}->
		?dbug({fetch,List}),

		ets:delete_all_objects(DTab),
		R=insertData(List,DTab,Max),
		Count=ets:info(DTab,size),
		notify_listeners({import,{source,csv},
                                 	 {size,Count},
                                 	 {status,R}}),
		?info({	{added,Count},
			{size,Count},
			{status,R},
			{memory,ets:info(DTab,memory)}}),

		State#ds_state{	data_count=Count,
				fetch_count=FetchCount+1,
                                next=ets:first(DTab)};
	
	eof->
		?info({endOfFile,csv}),
		State#ds_state{rq_count=RQ+1,next='$end_of_table',stop_tm=erlang:now()}
	end;
	
				

handle_end_of_table(State=#ds_state{data_tab=DTab,next=Next,rq_count=RQ,
				    importSpec=Import})->
	?dbug(endOfTable),
	notify_listeners('$end_of_table'),
	State#ds_state{rq_count=RQ+1,next='$end_of_table',stop_tm=erlang:now()}.

insertData(Data,DTab,Max)->

	%% this is perhaps not implemented in the most efficient
	%% manner. Insert all of the items into the ets table up
	%% to a maximum allowed ets table size. If we attempt to
	%% exceed this, we throw an error and return "truncated"
	%% to the calling function , otherwise we return the 
	%% actual table size

	R=(catch(lists:foldl(fun(Z,Acc) when Acc<Max->
			ets:insert(DTab,{Z}),ets:info(DTab,size);
		       (_Z,_Acc)->
				?warn({fileTruncated,{size,Max}}),
				throw(truncated)
		     end,
	 	    	ets:info(DTab,size),
		    Data))),

	case R of
	truncated->
		{ok,{truncated,{max_size,Max}}};

	N when is_integer(N)->
		{ok,{not_truncated,{size,N}}};

	E={error,Reason}->
		E;

	Error->
		{error,Error}
	end.

