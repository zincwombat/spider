-module(api).

-export([encode/3,
	 test/0]).

%
%	cmanager:start()->
%		POST	/op/cmanager
%	

encode(M,F,A)->
	Data=[{obj,[{module,M},
		    {function,F},
		    {args,A}]}],
	JsonData={obj,[{data,Data}]},
	rfc4627:encode(JsonData).

test()->
	encode(cmanager,start,[]).
