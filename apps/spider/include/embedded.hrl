-ifndef(_EMBEDDED_HRL).
-define(_EMBEDDED_HRL,true).
-include("globals.hrl").
-define(YAWS_DOCROOT,           ?BASEDIR++"/YAWS/wwwroot").
-define(YAWS_LOGDIR,            ?BASEDIR++"/YAWS/log").
-define(YAWS_PORT,              9010).
-define(YAWS_LISTEN,            {127,0,0,1}).
-define(YAWS_ANALYSIS_DIR,      "analysis").
-define(YAWS_ANALYSIS_ROOT,     ?YAWS_DOCROOT++"/"++?YAWS_ANALYSIS_DIR).
-define(YAWS_EBINDIR,           "/Users/tonyhobbins/OTP/lib/dg/ebin").
-define(YAWS_SERVERNAME,        "wallaby").
-define(YAWS_INCLUDES,          "/Users/tonyhobbins/OTP/lib/dg/include").
-endif.
