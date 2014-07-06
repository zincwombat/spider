-module(geoip).
-behaviour(gen_server).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2
        ]).

-export([start/0,
	 start_link/0,
	 stop/0]).

-export([setImportFile/1,
	 status/0,
	 reload/0,
	 lookup/1,
	 import/0]).

-sysloglevel(?TRACE_LEVEL).


-define(START_OPTIONS,          []).
-define(SERVERNAME,             geoip).
-define(SERVERACCESS,           {local,?SERVERNAME}).
-define(SERVERCALL,             ?SERVERNAME).
-define(LONGTMOUT,             	60000).

-record(state,{gtab,importFile,rcount=0}).

start_link()->
	start().

start()->
        gen_server:start_link(  ?SERVERACCESS,
				?SERVERNAME,
				[],
                                ?START_OPTIONS).

stop()->
        gen_server:call(?SERVERCALL,die).

init(_)->
	process_flag(trap_exit,true),
	
	GTab=ets:new(gtab,[named_table,ordered_set,public]),
	
	ImportFile=
	case application:get_env(spider,geoip_isp_filename) of
	{ok,File}->
		File;
	_->
		undefined
	end,
	{ok,#state{gtab=GTab,importFile=ImportFile}}.

setImportFile(Filename)->
	gen_server:call(?SERVERCALL,{setImportFile,Filename}).

import()->
	gen_server:call(?SERVERCALL,import,?LONGTMOUT).

%% convert this to a record

lookup({arr,{{addr,IP4={_,_,_,_}},{type,ip4}}})->
	lookup(IP4);

lookup({arr,IP4,ip4})->
	lookup(IP4);

lookup(IPAddress)->
	gen_server:call(?SERVERCALL,{lookup,IPAddress}).

status()->
	gen_server:call(?SERVERCALL,status).

reload()->
	gen_server:call(?SERVERCALL,reload,?LONGTMOUT).

handle_call(die,_,State)->
        {stop,normal,State};

handle_call(status,_,State)->
        {reply,{ok,State},State};

handle_call({setImportFile,Filename},_,State)->
        {reply,ok,State#state{importFile=Filename}};

handle_call({lookup,IP4},_,State=#state{rcount=0})->
	?critical({lookup,no_data}),
	{reply,{error,nodata},State};

handle_call({lookup,IP4},_,State)->
	Reply=
	case ip2long(IP4) of
	R={ok,IP4int}->
		i_getNearest(State#state.gtab,IP4int,IP4int);
	Other->
		Other
	end,
        {reply,Reply,State};

handle_call(import,_,State)->
	{Reply,NewState}=i_import(State),
	{reply,Reply,NewState};

handle_call(reload,_,State=#state{gtab=GTab})->
	%% TODO -- fix this to test for non-existence
	ets:delete_all_objects(gtab),
	Reply=i_import(State),
        {reply,Reply,State};

handle_call(Msg,_,State) ->
	%% unhandled message
	?info({unhandledCall,Msg,{state,State}}),
        {reply,ignored,State}.

handle_cast(Msg,State)->
	%% unhandled message
	?info({unhandledCast,Msg}),
        {noreply,State}.

handle_info(Msg,State)->
        ?info({unhandledInfo,Msg}),
        {noreply,State}.

terminate(Reason,_State) ->
	?critical({stopping,Reason}),
        ok.


%%=============================================================================	
%% Helper functions
%%=============================================================================	

i_import(State=#state{gtab=GTab,importFile=ImportFile}) when is_list(ImportFile)->
	case i_import(GTab,ImportFile) of
	R={ok,NumRecs}->
		?info({file_import,R}),
		{ok,State#state{rcount=NumRecs}};
	Other->
		?critical({file_import_failed,Other}),
		{Other,State#state{rcount=0}}
	end.

i_import(GTab,ImportFile)->
	%% need to fully qualify the file name:

	FullPath=code:priv_dir(spider)++"/"++ImportFile,
	case file:open(FullPath,[read]) of
	E={error,Reason}->
		?warn({file_error,{file,FullPath},E}),
		E;
	{ok,IoDevice}->
		?info({file_opened,{file,FullPath}}),
		{ok,RE}=re:compile("^(?<IPSTART>\\d+),(?<IPEND>\\d+),(?<ISP>.+)$"),
		i_readline(IoDevice,fun(Z,Count)->i_insert(GTab,RE,Z),Count+1 end,0)
	end.


i_readline(IoDevice,Proc,Acc)->
	case io:get_line(IoDevice,"") of
	eof->
		file:close(IoDevice),
		{ok,Acc};

	E={error,Reason}->
		%% TODO - throw error here
		E;

	Line->
		NewAcc=Proc(Line,Acc),
		i_readline(IoDevice,Proc,NewAcc)
	end.

i_insert(GTab,RE,Line)->
	case re:run(Line,RE,[{capture,all_but_first,list}]) of
	{match,[StartIP,EndIP,ISPName]}->
		ISP=[Y || Y <- ISPName, Y=/= $\"],
		ets:insert(GTab,{list_to_integer(StartIP),list_to_integer(EndIP),ISP}),
		ok;
	Other->
		io:format("error: [~p] encountered~n~p~n",[Other,Line]),
		ok
	end.

i_getNearest(GTab,OrigIP4int,IP4int) when is_integer(IP4int)->
	case ets:lookup(gtab,IP4int) of
	[]->
		%% TODO - ets:prev will fail if
		NewKey=ets:prev(GTab,IP4int),
		i_getNearest(GTab,OrigIP4int,NewKey);
	'$end_of_table'->
		[];

	[{Min,Max,ISP}] when Min =< OrigIP4int, Max >= OrigIP4int->
		{ok,ISP};

	Other->
		?error({lookup_failed,{ip4,OrigIP4int},Other}),
		[]
	end;

i_getNearest(GTab,OrigIP4int,_)->
	[].


address_fast([N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case N0 - $0 of
        N when N =< 255 ->
            address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast([N1, N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case list_to_integer([N1, N0]) of
        N when N =< 255 ->
            address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast([N2, N1, N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case list_to_integer([N2, N1, N0]) of
        N when N =< 255 ->
            address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast(L=[_N2, _N1, _N0], Num, 0) ->
    case list_to_integer(L) of
        N when N =< 255 ->
            Num bor N
    end;
address_fast(L=[_N1, _N0], Num, 0) ->
    case list_to_integer(L) of
        N when N =< 255 ->
            Num bor N
    end;
address_fast([N0], Num, 0) ->
    case N0 - $0 of
        N when N =< 255 ->
            Num bor N
    end;
address_fast(_N, _Num, _X) ->
    invalid_fast_address.


ip2long(Address) when is_integer(Address) ->
    {ok, Address};
ip2long(Address) when is_list(Address) ->
    case address_fast(Address, 0, 24) of
        N when is_integer(N) ->
            {ok, N};
        _ ->
            case inet_parse:address(Address) of
                {ok, Tuple} ->
                    ip2long(Tuple);
                Error ->
                    Error
            end
    end;
ip2long({B3, B2, B1, B0}) ->
    {ok, (B3 bsl 24) bor (B2 bsl 16) bor (B1 bsl 8) bor B0};
ip2long({W7, W6, W5, W4, W3, W2, W1, W0}) ->
    {ok, (W7 bsl 112) bor (W6 bsl 96) bor (W5 bsl 80) bor (W4 bsl 64) bor
         (W3 bsl 48) bor (W2 bsl 32) bor (W1 bsl 16) bor W0};
ip2long(<<Addr:32>>) ->
    {ok, Addr};
ip2long(<<Addr:128>>) ->
    {ok, Addr};
ip2long(_) ->
    {error, badmatch}.

