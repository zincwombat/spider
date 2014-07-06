-include("xmlinfo.hrl").

-define(URN_DG,                 "urn:datagatherer:v1").
-define(URN_DG_PFX,             "dg").
-undef(THIS_XMLNS).
-define(THIS_XMLNS,             xmlinfo=?XML(?URN_DG)).

-define(NSTATUS_V1_URI,	"urn:nstatus_v1").

-record(nstatus,	{version=?NSTATUS_V1_URI,
			 cm_state,
			 cc_state,
			 ds_state}).

-record(dg_info,	{?THIS_XMLNS,cm_state,cc_state,ds_state}).

-undef(THIS_XMLNS).
				

