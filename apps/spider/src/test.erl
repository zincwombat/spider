-module(test).
-export([go/0]).

go()->
	cmanager:reset(),
	cmanager:initConf(),
	cmanager:setImportFile("/Users/tonyhobbins/spider/upload/xaa","csv"),
	cmanager:setNumAgents(80),
	cmanager:import(),
	cmanager:setModuleFunction(urn_domain,webCheck,4),
	cmanager:setFilter(all),
%%	syslogger:set(rlogger,0),
	cmanager:run().
