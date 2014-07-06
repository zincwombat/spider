%% this is the global XML Manager server

-module(xmlManager).

-behaviour(gen_server).

-export([	init/1,
                handle_call/3,
                handle_cast/2,
                handle_info/2,
                terminate/2
                ]).

-export([	start/0,
                start/1,
		status/0,
                stop/0]).

-export(	[lookup/1,		%% lookup a particular xmlns
		 add/2,			%% add xmlinfo
		 delete/1,		%% delete a KV pair with key
		 purge/0,		%% remove all entries from the table
		 list/0	]).		

-define(START_OPTIONS,	[]).
-define(SERVERNAME,	?MODULE).
-define(SERVERACCESS,	{local,?SERVERNAME}).
-define(SERVERCALL,	?SERVERNAME).

-define(CONFIG_DEFAULTS,[]).

-define(CONFIGSAVE,	"xmlconfig.ets").	%% persistent configuration store

-include("debug.hrl").			%% debug support
-include("xmlinfo.hrl").		%% default configuration data

-record(state,{ctab}).	

%% PUBLIC API ------------------------------------------------------------------

start()->
	start([]).


start(Arg)->
        gen_server:start_link(  ?SERVERACCESS,
                                ?SERVERNAME,
                                [Arg],
                                ?START_OPTIONS).

stop()->
        gen_server:call(?SERVERCALL,die).


status()->
	%% return the status of the server
	gen_server:call(?SERVERCALL,status).

lookup(Key)->
	%% lookup the value corresponding to the key Key.
	%% Note, Key is of the form {Module,Key}
	gen_server:call(?SERVERCALL,{lookup,Key}).

add(Key,Value)->
	%% insert {Key,Value} in the table
	gen_server:call(?SERVERCALL,{add,Key,Value}).

delete(Key)->
	%% delete Key from the table
	gen_server:call(?SERVERCALL,{delete,Key}).

list()->
	%% list all the keys in the table
	gen_server:call(?SERVERCALL,list).

purge()->
	%% remove all keys from the table
	gen_server:call(?SERVERCALL,purge).


%% INTERNAL UTILITY ROUTINES ---------------------------------------------------

notify_listeners(Message)->
	configdb_evh:notify({?MODULE,Message}).
	
load()->
	load(?CONFIGSAVE).

load(File)->
	catch ets:file2tab(File).


save(Tab,File)->
	?dbug({etstab2File,File}),
	ets:tab2file(Tab,File).

%% CALLBACKS -------------------------------------------------------------------

init(_)->
	%% load the config database from the environment variables,
	%% if that doesn't work, we look for a hard coded default
	%% which resulted from the last load

	%% first create the ets table

	CTab=ets:new(ctab,[set,public]),

	%% initialise the state variable

	State=#state{ctab=CTab},

	%% get the initial configuration

	loadFromList(?CONFIG_DEFAULTS,CTab),

	process_flag(trap_exit,true),
	?info({pid,self()}),
	{ok,State}.

handle_call(die,_,State) ->
        {stop,normal,State};

handle_call({lookup,Key},_,State) ->
	Reply=handle_lookup(Key,State#state.ctab),
	{reply,Reply,State};

handle_call({lookup,Key,Default},_,State) ->
	Reply=
	case handle_lookup(Key,State#state.ctab) of
	{ok,R}->
		{ok,R};
	undefined->
		{ok,Default}
	end,
	{reply,Reply,State};

handle_call({delete,Key},_,State) ->
	notify_listeners({deleted,Key}),
	Reply=ets:delete(State#state.ctab,Key),
	{reply,Reply,State};

handle_call(defaults,_,State=#state{ctab=CTab}) ->
	Reply=loadFromList(?CONFIG_DEFAULTS,CTab),
	{reply,Reply,State};

handle_call(list,_,State) ->
	{reply,ets:tab2list(State#state.ctab),State};

handle_call({trace_level,Level},_,State) when is_integer(Level)->
	?tracelevel(Level),
	{reply,ok,State};

handle_call({add,Key,Value},_,State) ->
	CTab=State#state.ctab,
	Reply=srv_add(CTab,{Key,Value}),
	{reply,Reply,State};

handle_call(status,_,State=#state{ctab=CTab}) ->
        {reply,{State,ets:info(CTab)},State};

handle_call(reload,_,State) ->
	{Reply,NewState}=initConfig(State),
	{reply,Reply,NewState};

handle_call(purge,_,State=#state{ctab=CTab}) ->
	%% remove all the KV pairs from the table
	%% we don't notify subscribers
	Reply=ets:delete_all_objects(CTab),
	{reply,Reply,State#state{}};

handle_call({getModuleConfig,Module},_,State=#state{ctab=CTab}) ->
	%% get all KV pairs for which module=Module, assumes
	%% that keys are of the form {Module,Key'}

	Reply=ets:match_object(CTab,{{Module,'_'},'_'}),

	{reply,Reply,State};

handle_call(Msg,_,State) ->
	?info({unhandled_call,Msg}),
        {reply,ok,State}.

loadFromList(ConfigList,CTab)->
	?info({loadConfig,ConfigList}),
	lists:foreach(fun(Z)->srv_add(CTab,Z) end,ConfigList).

%% config file parsing routines ------------------------------------------------

initConfig(State=#state{ctab=CTab})->

	%% reload the config file passed to the erlang VM on boot
	%% first find the name of the config file:
	
	EVMArgs=init:get_arguments(),

	%% first see if a config file was passed as an argument to the
	%% erlang VM

	case lists:keysearch(config,1,EVMArgs) of
	{value,{config,[ConfigFile]}}->
		%% ConfigFile is the name of the config file
		%% passed on EVM boot

		?info({loadConfigFile,ConfigFile}),

		%% open the file and try and load....
		
		case file:consult(ConfigFile) of
		{ok,[C]}->
			%% OK, we can load
			loadConfig(C,CTab),
			{ok,State#state{}};

		E={error,_Reason}->
			%% for some reason, we cannot open this file
			%% should never get here, since the EVM won't
			%% start unless the config file can be loaded!
			?critical({readConfigFileFail,E}),
			{stop,{error,{bad_config,ConfigFile,E}}}
		end;

	_->
		%% there was no config file passed as an argument to
		%% EVM, see if we can reload from a previously saved
		%% copy of the ets table ?CONFIGSAVE

		?warn(noConfigFile),

		case load() of
		{ok,Tab}->
			%% we have converted the ets file into a table
			%% now we have to substitute the existing table
			%% with this one!
			%% we should send notifications ... TBD

			%% delete the existing ets table

			OldConfig=ets:tab2list(Tab),

			?info({loadedPreviousConfig,OldConfig}),

			loadFromList(OldConfig,CTab),

			ets:delete(Tab),

			{ok,State#state{}};

		Error->
			%% we have to revert to any harcoded defaults now
			?warn({configFileLoadFile,Error}),

			NewCTab=ets:new(ctab,[set,public]),
			loadFromList(?CONFIG_DEFAULTS,NewCTab),
			{ok,State#state{ctab=NewCTab}}
		end
	end.

loadConfig(Config,CTab) when is_list(Config)->
	%% we have a termlist of the form
	%% [{Application,[{config,[{Module,{Key,Value},...]},_ ....
	%% we process the applications one by one ....

	lists:foreach(fun(Z)->loadAppConfig(Z,CTab) end,Config).

loadAppConfig({App,AppEnvList},CTab) when is_list(AppEnvList)->
	?info({searchingConfigFile,App}),
	%% we need to search AppEnvList for a tuple of the form
	%% {config,ConfigList}
	case lists:keysearch(config,1,AppEnvList) of
	{value,{config,ConfigList}}->
		lists:foreach(fun(Z)->i_loadConfig(App,Z,CTab) end,ConfigList);
	_->
		?warn({noConfigFound,App})
	end.

i_loadConfig(App,{Module,ModuleConfig},CTab)->
	?info({configFound,{App,Module}}),
	lists:foreach(fun(Z)->srv_add(CTab,{App,{Module,Z}}) end,ModuleConfig);

i_loadConfig(_App,_,_)->
	ignore.

%% END -------------------------------------------------------------------------

srv_add(CTab,{_Application,{Module,{Key,Value}}})->
	srv_add(CTab,{{Module,Key},Value});

srv_add(CTab,{Key,Value})->
	Mode=
        case ets:lookup(CTab,Key) of
        []->
		notify_listeners({added,Key,Value}),
		?dbug({insertNew,{Key,Value}}),
        	ets:insert(CTab,{Key,Value}),
                new;

        [{Key,Value}]->
                no_change;

        [{Key,OldValue}]->
		notify_listeners({changed,Key,{{old,OldValue},{new,Value}}}),
		?dbug({insertChange,{Key,Value}}),
        	ets:insert(CTab,{Key,Value}),
                updated
        end,
	{ok,Mode};

srv_add(_CTab,Other)->
	?dbug({unhandled,Other}).

handle_lookup(Key,Table)->
	handle_lookup2(ets:lookup(Table,Key)).

handle_lookup2([{_Key,Value}])->
	{ok,Value};

handle_lookup2([])->
	undefined.	

handle_cast(Msg,State)->
	?info({unhandledCast,Msg}),
        {noreply,State}.

handle_info({'EXIT',Pid,Reason},State)->
	?info({exit,Pid,Reason}),
        {noreply,State};

handle_info(Msg,State)->
	?info({unhandledInfo,Msg}),
        {noreply,State}.

terminate(Reason,State)->
	save(State#state.ctab,?CONFIGSAVE),
	?critical({stopping,Reason}),
	ok.
