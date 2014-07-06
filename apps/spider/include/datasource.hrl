-include("xmlinfo.hrl").

-undef(THIS_XMLNS).
-define(THIS_XMLNS,             xmlinfo=?XML(?URN_DS)).
-define(SCHEMALOC,       	[]).
-define(XSL,       		[]).

%% this is the record definition for the gen_server state 

-record(ds_state,  {
		rq_count,	%% request count
               	data_tab,      	%% reference to ETS table
		data_count,    	%% number of items in table
		importSpec=#import{}, %% imported source
		next,          	%% next data item to return
		start_tm,      	%% time of first read
		stop_tm,       	%% time of last read
		max_size,      	%% max allowed size
		fetch_count	%% num fetches from filesource
}).

-define(DS_TMOUT,                  60000).
-define(DS_LONGTMOUT,              600000).
-define(DS_MAXDATA,                100000). %% default max size

%% this is the record definition used by the XML serialiser

-record(ds_info,	{?THIS_XMLNS,rq_count,data_count,import_spec,next,start_tm,stop_tm,max_size}).
				
-record(rq_count,	{?THIS_XMLNS,rq_count}).
-record(data_count,	{?THIS_XMLNS,data_count}).
-record(import_spec,	{?THIS_XMLNS,import_spec}).
-record(next,		{?THIS_XMLNS,next}).
-record(start_tm,	{?THIS_XMLNS,start_tm}).
-record(stop_tm,	{?THIS_XMLNS,stop_tm}).
-record(max_size,	{?THIS_XMLNS,max_size}).

-undef(THIS_XMLNS).
