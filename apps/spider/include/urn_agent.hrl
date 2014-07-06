-include("xmlinfo.hrl").

-undef(THIS_XMLNS).
-define(THIS_XMLNS,             xmlinfo=?XML(?URN_AG)).
-define(XMLNSATTR,		{'xmlns:ag',?URN_AG}).

%% this is the record definition for the gen_server state 


%%-record(agentConf,{     
%%                        invoke,                 %% fun/1 that does the work
%%                        get,                    %% fun/1 that gets data
%%                        logger,                 %% fun/1 that logs the data
%%                        timeout,                %% max time to wait for invoke
%%                        sleep,                  %% sleep in mS between invokes
%%                        max_rq                  %% max allowed agent requests
%%                        }).



%% this is the record definition used by the XML serialiser

-record(ag_info,	{?THIS_XMLNS,invoke,get,logger,timeout,sleep,max_rq}).
-record(ag_payload,	{?THIS_XMLNS,agentId,timestamp,payload}).
				
-record(invoke,	{?THIS_XMLNS,invoke}).
-record(get,	{?THIS_XMLNS,get}).
-record(logger,	{?THIS_XMLNS,logger}).
-record(timeout,{?THIS_XMLNS,timeout}).
-record(sleep,	{?THIS_XMLNS,sleep}).
-record(max_rq,	{?THIS_XMLNS,max_rq}).

-record(agentId,{?THIS_XMLNS,agentId}).

-undef(THIS_XMLNS).
