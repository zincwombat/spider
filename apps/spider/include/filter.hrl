-ifndef(_FILTER_HRL).
-define(_FILTER_HRL,true).

-define(FILTER_V1_URI,	'urn:filter_v1').

-record(file_type, {	key,
			desc,
			mime_type}).
	
-record(filter_opt,{	key,
			type,
			desc}).

-define(FILETYPE_XML,	#file_type{key=xml,
				   desc="XML Format",
				   mime_type="text/xml"}).

-define(FILETYPE_CSV,	#file_type{key=csv,
				   desc="CSV Format",
				   mime_type="text/plain"}).

-define(FILETYPE_ERL,	#file_type{key=erl,
				   desc="Erlang Term Format (Native Erlang)",
				   mime_type="text/plain"}).

-endif.
