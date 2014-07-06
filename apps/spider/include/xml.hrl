-define(XML_HEADER,"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>").
-define(LEFT,"<").
-define(RIGHT,">").
-define(SLASHLEFT,"</").
-define(SPACE," ").

-record(nsinfo,{
        element,
        prefix=[],
        uri,
	cmodel}).

