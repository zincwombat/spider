-ifndef(SYSTEMNS).
-define(SYSTEMNS,		"urn:erlang").
-define(PREFIX,			"e").
-define(URN_CM,                 "urn:cmanager:v1").
-define(URN_CM_PFX,             "cm").
-define(URN_CC,                 "urn:controller:v1").
-define(URN_CC_PFX,             "cn").
-define(URN_AG,                 "urn:agent:v1").
-define(URN_AG_PFX,             "ag").
-define(URN_DS,                 "urn:datasource:v1").
-define(URN_DS_PFX,             "ds").



-record(xmlinfo,		{xmlns}).
-record(xstype,			{tag,type,value,attributes=[]}).

-record(xmlns,			{xmlns}).
-record(object,			{xmlinfo=#xmlinfo{},payload}).
-record(error,			{xmlinfo=#xmlinfo{xmlns=?SYSTEMNS},description}).

-define(MKERROR(CLASS,DESC),	#error{description=DESC}).

-define(XML(NS),		#xmlinfo{xmlns=NS}).

-define(XSINT(TAG,V),		#xstype{tag=TAG,type=integer,value=V}).
-define(XSSTRING(TAG,A,V),	#xstype{tag=TAG,type=text,attributes=A,value=V}).
-define(XSIP4(TAG,V),		#xstype{tag=TAG,type=ip4address,value=V}).
-define(XSERLTIME(TAG,V),	#xstype{tag=TAG,type=erltime,value=V}).
-define(XSERLFUN(TAG,V),	#xstype{tag=TAG,type=erlfun,value=V}).
-define(XSNULL(TAG),		#xstype{tag=TAG,type=null}).
-define(XSCOMPLEX(TAG,A,V),	#xstype{tag=TAG,type=complex,attributes=A,value=V}).
-define(XSSEQ(TAG,A,V),		#xstype{tag=TAG,type=sequence,attributes=A,value=V}).
-endif.
