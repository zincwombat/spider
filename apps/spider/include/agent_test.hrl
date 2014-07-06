-define(TESTCONF2,       #agentConf{
			invoke=fun(Z)->check1:invoke(Z) end,
                         get=fun(Z)->datasource:dget_a(Z) end,
                         logger=fun(Z)->rlogger_evh:notify(Z) end,
                         timeout=15000,
                         sleep=0,
                         max_rq=unlimited}).

-define(TESTCONF,       #agentConf{invoke=fun(Z)->mine:analyse(Z,delegatedns) end,
                         get=fun(Z)->datasource:dget_a(Z) end,
                         logger=fun(Z)->rlogger_evh:notify(Z) end,
                         timeout=15000,
                         sleep=0,
                         max_rq=1000000}).

-define(NULLCONF,       #agentConf{invoke=null,
                         get=fun(Z)->datasource:dget_a(Z) end,
                         logger=fun(Z)->rlogger_evh:notify(Z) end,
                         timeout=15000,
                         sleep=0,
                         max_rq=1000000}).

%%-define(TESTDATA,	"/tmp/big.dmx").
-define(TESTDATA,	"/home/thobbins/OTP/monitor/src/microsoft.1000.domains").
%%-define(TESTDATA,	"/home/share/analyser/import/sean03112004.dmx").
