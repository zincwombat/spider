%%%-------------------------------------------------------------------
%%% @copyright 2005 Tail-F Systems AB
%%% @version {$Id: regex.erl 2902 2006-05-17 11:49:41Z jb $}
%%% @doc linked in driver to POSIX regex
%%% Driver which interfaces by binaries to the libc regex code
%%% better efficency than erlang builtin and also more 
%%% functions
%%% @end
%%% @end
%%%-------------------------------------------------------------------
-module(posregex).

%%% external exports
-export([
	 load_driver/0,
	 compile/1, 
	 compile/2,
	 exec/2,
	 exec/3, 
	 match/2,
	 match/3, 
	 cmatch/2,
	 cmatch/3,
	 free/1]).


%% @spec load_driver() -> ok
%% 
%% @doc loads the linked in driver. 
%% It's the responsibility
%% of the caller to initially call this function once.

    
load_driver() ->
    Dir = filename:join([filename:dirname(code:which(posregex)),"..", "priv"]),
    erl_ddll:load_driver(Dir, "posregex_drv").



%% @type compopts() = [extended|icase|newline|nosub]
%% @type execopts() = [notbol|noteol]
%% @type matchhits() =  [{BegPos::integer(), Len::integer()}]



%% internal defines
-define(COMPILE, $c).
-define(EXEC,    $e).
-define(MATCH,   $m).


%% @spec compile(Re) -> term()
%% @equiv compile(Re,[])
compile(Re) ->
    compile(Re, []).
%% @spec compile(Re::binary(), Options::compopts()) ->
%% {ok,port()} | {error, atom()}
%% @doc Compile a regexp
%% Compile a regexp and return a port, The port must be free()'d
%% when no longer needed. There will be memory in the driver associated
%% to the port
compile(Pat, Options) when is_binary(Pat), is_list(Options) ->
    Flags = copt_to_flag(Options,0),
    P = open_port({spawn, 'posregex_drv'}, [binary]), 
    erlang_port_command(P, <<?COMPILE:32, Flags:32, Pat/binary, 0:8>>),
    receive
	{Port, ok} ->
	    {ok, Port};
	{_Port, Err} ->
	    {error, Err}
    end.

%% @spec match(port(), binary()) -> ok | {error, nomatch|atom()}
%% @equiv match(port, binary(), [])
match(Port, Str) ->
    match(Port, Str, []).

%% @spec match(port(), binary(), execopts()) -> ok | {error, nomatch|atom()}
%% @doc Check if Str matches
match(Port, Str, Opts) when is_binary(Str) ->
    Flags = eopt_to_flag(Opts, 0),
    erlang_port_command(Port, <<?MATCH:32, Flags:32, Str/binary, 0>>),
    receive
	{Port, ok} ->
	    ok;
	{Port, Err} ->
	    {error, Err}
    end.


%% @spec exec(P, B) -> term()
%% @equiv exec(P, B, [])
exec(Port, Str) ->
    exec(Port, Str, []).

%% @spec exec(port(), binary(), execopts()) -> {ok, Hits::matchhits()} | {error, nomatch|atom()}
%% @doc Check if Str matches and return where the match occured
%% When using extended regmatches, the list may be longer that 1
%% since all the submatches are returned as well
exec(Port, Str, Opts) ->
    Flags = eopt_to_flag(Opts, 0),
    erlang_port_command(Port, <<?EXEC:32, Flags:32, Str/binary, 0>>),
    receive
	{Port, ok, Hits} -> {ok, Hits};
	{Port, Err} -> {error, Err}
    end.


%% @spec cmatch(Pat, Bstr) -> term()
%% @equiv cmatch(Pat, Bstr, [])
cmatch(Pat, Str) ->
    cmatch(Pat, Str, []).
%% @spec(Pat:binary(), Bstr:binary(), compopts()) ->
%% {ok, Hits::matchhits()} | {error, nomatch|atom()}
%% @doc compile, execmatch and free in one go
cmatch(Pat, Str, CompOpts) when is_binary(Str), is_binary(Pat)  ->
    case compile(Pat, CompOpts) of
	{ok, Port} ->
	    Ret = exec(Port, Str, []),
	    free(Port),
	    Ret;
	Err ->
	    Err
    end.
    
%% @spec free(Port::port()) -> ok
%% @doc A compiled regex is a port, which has data structures associated
%% to it in the linked in driver. This function closes the port
%% and all data associated to the compiled regex will be freed
free(X) ->
    unlink(X),
    exit(X, stop),
    ok.



erlang_port_command(P, C) ->
    erlang:port_command(P, C).


-define(REG_EXTENDED, 1).
-define(REG_ICASE,    1 bsl 1).
-define(REG_NEWLINE,  1 bsl 2).
-define(REG_NOSUB,    1 bsl 3).


-define(REG_NOTBOL, 1).
-define(REG_NOTEOL, 1 bsl 1).


copt_to_flag([extended|Tail], F) -> copt_to_flag(Tail, F bor ?REG_EXTENDED);
copt_to_flag([icase|Tail], F) -> copt_to_flag(Tail, F bor ?REG_ICASE);
copt_to_flag([newline|Tail], F) -> copt_to_flag(Tail, F bor ?REG_NEWLINE);
copt_to_flag([nosub|Tail], F) -> copt_to_flag(Tail, F bor ?REG_NOSUB);
copt_to_flag([], F) -> F.
    
eopt_to_flag([notbol|Tail], F) -> eopt_to_flag(Tail, F bor ?REG_NOTBOL);
eopt_to_flag([noteol|Tail], F) -> eopt_to_flag(Tail, F bor ?REG_NOTEOL);
eopt_to_flag([], F) -> F.
    
    
