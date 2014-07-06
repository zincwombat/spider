-module(syaws).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").
-include("spider_config.hrl").

-export([start/0,
	 restart/0,
	 stop/0]).

-export([status/0]).

get_config(Key)->
        get_config(Key,undefined).

get_config(Key,Default)->
        case application:get_env(spider,Key) of
        {ok,Value}->
                Value;
        _->
                Default
        end.


start()->
	YAWS_DOCROOT=get_config(yaws_docroot),
        YAWS_LOGDIR=get_config(yaws_logdir,"/tmp"),
        YAWS_EBINDIR=get_config(yaws_ebindir,["."]),
        YAWS_LISTEN=get_config(yaws_listen,{127,0,0,1}),
        YAWS_PORT=get_config(yaws_port,9000),
        YAWS_ID=get_config(yaws_id,spider),

	YAWS_SCONF=[{docroot,YAWS_DOCROOT},{port,YAWS_PORT},{listen,YAWS_LISTEN},{appmods,[{"/engine",engine}]}],
        YAWS_GCONF=[{logdir,YAWS_LOGDIR},{ebin_dir,YAWS_EBINDIR},{id,YAWS_ID}],

	{ok,SCList,GC,ChildSpecs}=yaws_api:embedded_start_conf(YAWS_DOCROOT,YAWS_SCONF,YAWS_GCONF,YAWS_ID), 
	?info({yaws_embedded_config,{sclist,SCList},{gc,GC},{childspecs,ChildSpecs}}),
	put('cspec',ChildSpecs),
	[supervisor:start_child(?SPIDER_SUPERVISOR_NAME,Ch) || Ch <- ChildSpecs], 
	yaws_api:setconf(GC,SCList),

	%% added TOH 21/6/2014

	cmanager:reset(),
	cmanager:initConf(),
	geoip:import().
	


restart()->
	ok.

stop()->
	ok.

status()->
	?info({childSpecs,get('cspec')}),
	supervisor:which_children(?SPIDER_SUPERVISOR_NAME).
