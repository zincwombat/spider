-module(mail).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([go/0]).



-define(SMTP,"203.27.226.133").

-define(FROM,"mail@tonyhobbins.com").
-define(TO,"mail@tonyhobbins.com").
-define(MSG,"this is the message body").

go()->
	{ok,Pid} = smtp_fsm:start(?SMTP),
	?dbug({pid,Pid}),
	EHLO=smtp_fsm:ehlo(Pid),
	?dbug({ehlo,EHLO}),
	F=smtp_fsm:features(Pid),
	?dbug({features,F}),
	Msg=email_msg:simp_msg(?FROM,?TO,"No Subject",?MSG),
	?dbug({message,Msg}),
	M=smtp_fsm:sendemail(Pid,?FROM,?TO,Msg),
	?dbug({sendResponse,M}),
	smtp_fsm:close(Pid).
	
	
