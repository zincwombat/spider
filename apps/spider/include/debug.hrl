-define(TRACE_DEBUG,		0).
-define(TRACE_INFO,		1).
-define(TRACE_WARN,		2).
-define(TRACE_CRITICAL,		3).

-define(TRACE_DEFAULT,		1).

-define(tracedebug,		put(trace_level,?TRACE_DEBUG)).
-define(traceinfo,		put(trace_level,?TRACE_INFO)).

-define(DEBUG, 0).

-define(trace(Str),	syslogger:trace(?MODULE,{{line,?LINE},Str})).
-define(dbug(Str),	syslogger:debug(?MODULE,{{line,?LINE},Str})).
-define(info(Str),	syslogger:info(?MODULE,{{line,?LINE},Str})).
-define(warn(Str),	syslogger:warn(?MODULE,{{line,?LINE},Str})).
-define(critical(Str),	syslogger:critical(?MODULE,{{line,?LINE},Str})).
-define(tracelevel(Lvl),syslogger:add(?MODULE,Lvl)).


-ifdef(DEBUG).
-define(debug(Str,X),		trace:log(?MODULE,{?MODULE,?LINE},Str,X)).
-define(debug2(Key,Str,X),	trace:log(Key,{?MODULE,?LINE},Str,X)).
-else.
-define(debug(X, Y), true).
-endif.

-define(location,	{location,{module,?MODULE},{line,?LINE}}).

-define(error(E),	{error,{?location,{detail,E}}}).

-define(echo(Thread,Message,State),	trace:log(?MODULE,{?MODULE,?LINE},
				"<<~p>>:message ~p received in state ~p~n",
				[Thread,Message,State])).

-define(ignore(Thread,Message,State),	trace:log(?MODULE,{?MODULE,?LINE},
				"<<~p>>:message ~p ignored in state ~p~n",
				[Thread,Message,State])).
