-include("xmlinfo.hrl").

-define(URN_SPIDER,                 "urn:spider:v1").
-define(URN_SPIDER_PFX,             "sp").
-undef(THIS_XMLNS).
-define(THIS_XMLNS,             xmlinfo=?XML(?URN_SPIDER)).

-define(SPIDER_V1_URI, "urn:spider_v1").
-define(SPIDERDATA_V1_URI, "urn:spiderdata_v1").


%% definition of the SPIDER object record -- this is extensible

-record(spider,		{version=?SPIDER_V1_URI,
			 root_domain,
			 props=[],
			 data=[]}).

-record(spider_data,	{version=?SPIDERDATA_V1_URI,
			 url,
			 props=[],
			 data=[]}).

-undef(THIS_XMLNS).
				

