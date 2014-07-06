-module(htmlutils).


-include("debug.hrl").
-export([form/2]).

-record(form,{name,action,method}).

form(F,Body) when is_record(F,form)->
	{form,parseOptions(F),Body}.

parseOptions(#form{name=Name,action=Action,method=Method})->
	[{name,Name},{action,Action},{method,Method}];

parseOptions(F)->
	[].

