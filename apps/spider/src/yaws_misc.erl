-module(yaws_misc).

-export([selectFileSuffix/2,
	 selectFile/6,
	 selectFile/7,
	 simpleForm/4,
	 simpleForm/5,
	 radioForm/6,
	 multiForm/7,
	 multiForm/8,
	 radio/2,
	 radio/3,
	 rline/3,
	 rline2/3,
	 check/2,
	 check/3,
	 hidden/2,
	 submit/1,
	 input/2
	 ]).

-include("debug.hrl").

-define(DEFAULT_WIDTH,	"100").

selectFileSuffix(Dir,S="."++Suffix) when list(Suffix)->
	selectFileSuffix(Dir,Suffix);

selectFileSuffix(Dir,Suffix) when list(Suffix)->
	Regexp=lists:flatten(io_lib:format("[.]~s$",[Suffix])),
	selectFileRegexp(Dir,Regexp).


selectFileRegexp(Dir,Regexp) when list(Dir)->
	%% allow the user to select a file from Dir which has
	%% a name that matches the regular expression RegExp

	case file:list_dir(Dir) of
	{ok,Files}->
		lists:filter(fun(Z)->regexpMatch(Z,Regexp) end,Files);
	Other->
		Other
	end.

regexpMatch(Domain,Regexp)->
	case regexp:match(Domain,Regexp) of
	{match,_,_}->
		true;
	_->
		false
	end.

selectFile(Op,Dir,Suffix,Name,Action,Method,Prompt)->
	Files=selectFileSuffix(Dir,Suffix),
	selectFile(Op,Files,Name,Action,Method,Prompt).


selectFile(Op,Files,Name,Action,Method,Prompt)->
	{form,[	{action,Action},
		{method,Method}],
	      [ Prompt,
		hidden("op",Op),
		{select,[{name,"filename"},{size,"1"}],optionList(Files)},
		submit(Name)
		]
	}.

simpleForm(Op,FieldName,Action,Method)->
	simpleForm(Op,FieldName,Action,Method,[]).

simpleForm(Op,FieldName,Action,Method,Prompt)->
	{form, 	[{method,Method},{action,Action}],
		[Prompt,
		 submit(Op),
		 hidden("op",Op),
		 input(FieldName)
		]}.

multiForm(Op,FieldName,Rows,Cols,Action,Method,Prompt)->
	multiForm(Op,FieldName,Rows,Cols,Action,Method,Prompt,[]).

multiForm(Op,FieldName,Rows,Cols,Action,Method,Prompt,Extra)->
	{form,  [ {method,Method},
		  {action,Action}],
                [ {textarea,[{rows,Rows},{cols,Cols},{name,FieldName}],[]},
		  hidden("op",Op),
		  Extra,
		  submit(Op)
                ]}.

radioForm(Op,FieldName,Action,Method,Prompt,Extra)->
	 {form,  [ {method,Method},
                  {action,Action}],
                [ hidden("op",Op),
                  Extra,
                  submit(Op)
                ]}.

input(Name)->
	input(Name,?DEFAULT_WIDTH).

input(Name,Width)->
	{input,[{width,Width},
                {name,Name}],[]}.
	
submit(Prompt)->
	{input,[{type,"submit"},{value,Prompt}],[]}.		 

hidden(Name,Value)->
	{input,[{type,"hidden"},{name,Name},{value,Value}],[]}.

optionList(List)->
	lists:map(fun(Z)->{option,[],Z} end,List).
	
radio(Name,Options)->
	radio(Name,Options,[],[]).

radio(Name,Options,Prompt) when list(Options)->
	[ {p,[],Prompt},
           lists:map(fun(Z)->{input,[{type,"radio"},
                                  {name,Name},
                                  {value,Z}],Z} end,Options)
	].

radio(Name,Options,Prompt,Checked) when list(Options)->
	[ {p,[],Prompt},
           lists:map(fun(Z)->{input,[{type,"radio"},
                                  {name,Name},
                                  {value,Z}],Z} end,Options)
	].


rline(Name,Values,Buttons) when is_list(Values),is_list(Buttons)->
	%% Values is [{Value,Label|,....]
	lists:map(fun(Z)->rline(Name,Z,Buttons) end);

rline(Name,{Value,Label},Buttons) when is_list(Buttons)->
	lists:map(fun(Z)->rline2(Name,{Value,Label},Z) end).

rline2(Name,{Value,Label},Value)->
	{input,[{type,"radio"},{name,Name},{value,Value},{checked,"True"}],Label};

rline2(Name,{Value,Label},Other)->
	{input,[{type,"radio"},{name,Name},{value,Value}],Label}.
	
check(Name,Options)->
	check(Name,Options,[]).

check(Name,Options,Prompt) when list(Options)->
	[ {p,[],Prompt},
           lists:map(fun(Z)->{input,[{type,"checkbox"},
                                  {name,Name},
                                  {value,Z}],Z} end,Options)
	].
		
