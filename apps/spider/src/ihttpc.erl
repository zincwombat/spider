-module(ihttpc).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").
-export([
	 vhead/1,
	 xhead/1,
	 head/1,	%% HTTP Head request
	 head/2,	%% HTTP Head request, UA Headers supplied
	 head/3,	%% HTTP Head request, UA Headers + callback supplied
	 hget/1,
	 webStats/1,
	 hget/2,
	 hget/3,
	 tokenCheck/2,
	 get_until/3,
	 spider/1,
	 test/0]).

-include("http.hrl").
-include("facetmap_web_v1.hrl").
-sysloglevel(?TRACE_WARN).

-define(TCP_CONNECT_TMOUT,30000).	%% 30 second timeout
-define(HTTP_TMOUT,45000).		%% 45 second timeout
-define(MAX_CNAME_LOOP,5).		%% max DNS CNAME lookups
-define(MAX_REDIR,5).			%% max HTTP redirects

-define(EMAIL_REGEXP,	"mailto:[a-z0-9A-Z_]+\@[a-z0-9A-Z_.-]+").
-define(HTTP_REGEXP,	"http:[a-z0-9A-Z_/.]+").

%% TBD:: we need to indicate what sort of analysis is to be done.
%% i.e. specify the facetmap?????


%% some basic content checking ....

%% TBD - add detection of foreign email links
%% TBD - add detection of local email links
%% TBD - same as above but for http:
%% TBD - detect META tags


vhead(Domain)->
	Method=head,
	URL="http://" ++ Domain,
	Headers=[],
	Request={URL,Headers},
	HTTPOptions=[{autoredirect,false}],
	Options=[],
	httpc:request(Method,Request,HTTPOptions,Options).


test()->
	session(#httpRequest{url="http://tonyhobbins.com",
				 proxy=none,
				 method=head,
				 headers=[]}).

tokenCheck(link,URL)->
	tokenCheck(?HTTP_REGEXP,URL);

tokenCheck(email,URL)->
	tokenCheck(?EMAIL_REGEXP,URL);

tokenCheck(REGEXP,URL)->
	case hget(URL) of
	{ok,{{_,200,_},H,Body}}->
		%% we have some content returned ...
		tokenCheck(REGEXP,URL,Body);

	{redo,URL2}->
		tokenCheck(REGEXP,URL2);

	Other->
		{error,Other}
	end.

tokenCheck(REGEXP,URL,Content)->
	case regexp:matches(Content,REGEXP) of
       	M={match,Matches}->
       		{URL,displayMatch(Matches,Content)};

	E={error,Reason}->
       		E
	end.


webStats(URL)->
	case hget(URL,[{"connection","close"}]) of
	Z={ok,{{_,200,_},H,Body}}->
		?dbug({body,Body}),
		%% perform some analysis on the body .....
		R=facetmap_webcontent_v1:classify(URL,Z),

		{webStats,R};

	Z={ok,{{_,Code,_},_H,_Body}}->
		{error,{httpCode,Code}};

	{redo,URL2}->
		webStats(URL2);

	Other->
		{error,Other}
	end.

spider(URL)->
	case hget(URL,[{"connection","close"}]) of
	Z={ok,{{_,200,_},H,Body}}->
		%% perform some analysis on the body .....
		R=facetmap_webcontent_v1:spider(URL,Z),
		{spider,R};

	{redo,URL2}->
		spider(URL2);

	Other->
		{error,Other}
	end.
	

displayMatch([],String)->
	[];

displayMatch(M,String) when is_list(M)->
	lists:map(fun(Z)->displayMatch(Z,String) end,M);

displayMatch({StartPos,Length},String)->
	string:substr(String,StartPos,Length).


%% HTTP HEAD Request, with or without HEADERS provided
%% #httpRequest is of the form
%%	url,
%%	method = [head|get],
%%	proxy = [none|#proxy{ip,port}]
%%	headers = default UA (defined in http.hrl)
%%	http_tmout = default 10000 ms (defined in http.hrl)
	
%% headers provided ----

xhead(X="http://"++URL)->
	httpc:request(head,{X,[]},[{autoredirect,false}],[]);

xhead(X="https://"++URL)->
	httpc:request(head,{X,[]},[{autoredirect,false}],[]);

xhead(URL)->
	httpc:request(head,{"http://"++URL,[]},[{autoredirect,false}],[]).


head(URL)->
	head(URL,[],undefined).

head(URL,H)->
	head(URL,H,undefined).

head(URL="http://" ++ Rest,H,Module)->
	?dbug({httpHead,{url,URL},{headers,H},{module,Module}}),
	M=
	case Module of
	undefined->
		Module;
	F->
		fun(Z)->apply(Module,classify,[URL,Z]) end
	end,
	session(#httpRequest{url=URL,
			     proxy=none,
			     method=head,
			     headers=H,
			     module=M,
			     redir_cnt=0});

head(URL="https://" ++ Rest,_H,_Module)->
	{error,{ssl_not_implemented,URL}};

head(URL,H,Module)->
	head("http://"++URL,H,Module).

hget(URL)->
	hget(URL,[],undefined).

hget(URL,H)->
	hget(URL,H,undefined).

hget(URL="HTTP://" ++ Rest,H,Module)->
	hget("http://" ++ Rest,H,Module);

hget(URL="http://" ++ Rest,H,Module)->
	?dbug({httpGet,{url,URL},{headers,H},{module,Module}}),
	M=
	case Module of
	undefined->
		Module;
	F->
		fun(Z)->apply(Module,classify,[URL,Z]) end
	end,
	session(#httpRequest{url=URL,
			     proxy=none,
			     method=get,
			     headers=H,
			     module=M,
			     redir_cnt=0});

hget(URL="https://" ++ _Rest,_H,_M)->
	{error,{ssl_not_implemented,URL}};

hget(URL,H,Module)->
	hget("http://"++URL,H,Module).


%% redirection analysis 

redirect(URL,RURL="http://"++_Rest)->
	X1=xuri:parse(URL),
	X2=xuri:parse(RURL),
	i_redirect(X1,X2);

redirect(URL,RURL="https://"++_Rest)->
	X1=xuri:parse(URL),
	X2=xuri:parse(RURL),
	?dbug({calling,X1,X2}),
	i_redirect(X1,X2);

redirect(URL,RURL)->
	{?REDIRINTERNAL,RURL}.

i_redirect(_,{error,Reason})->
	{?REDIRUNKNOWN,badarg};

i_redirect({S1,D1,P1,Path1},RURL={S1,"www."++D1,P2,Path2})->
	%% case 1, redirect to www subdomain
	{?REDIRINTERNAL,RURL};

i_redirect({S1,D1,P1,Path1},RURL={S1,D1,P2,Path2})->
	%% case 1b, redirect to www subdomain
	{?REDIRINTERNAL,RURL};

i_redirect({S1,D1,P1,Path1},RURL={S2,D2,P2,Path2}) when D1 /= D2->
	%% case 2, external redirect
	{?REDIREXTERNAL,RURL};

i_redirect({S1,D1,P1,Path1},RURL={https,D1,P2,Path2})->
	%% case 3, SSL internal redirect
	{?REDIRINTERNALSSL,RURL};

i_redirect({S1,D1,P1,Path1},RURL={https,D2,P2,Path2})->
	%% case 4, SSL external redirect
	{?REDIREXTERNALSSL,RURL};

i_redirect({S1,D1,P1,Path1},RURL={S1,D1,P1,Path2})->
	%% case 5, internal redirect
	{?REDIRINTERNAL,RURL}.

makeurl(http,Host,80,Path)->
	"http://"++Host++ensureSlash(Path).

ensureSlash(Path="/"++Rest)->
	Path;

ensureSlash(Path)->
	"/"++Path.



session(#httpRequest{url=URL,redir_cnt=RedirCnt}) when redir_cnt==?MAX_REDIR->
	?warn({redir_cnt_exceeded,URL,RedirCnt}),
	?BADARG;
				
session(HTTPRequest=#httpRequest{module=M,
				 url=URL,
				 redir_cnt=RedirCnt}) when is_function(M)->
	%% invokes a session as specified by HTTPRequest=#httpRequest
	%% get the #proxy{} information 

	#proxy{ip=Domain,port=Port}=server(HTTPRequest),

	%% create a fun that takes an IP address as input and pass
	%% to a handler

	F=fun(Z)->i_session(HTTPRequest,Z) end,

	I=(catch itcp_invoke:request(Domain,Port,F)),
	case I of
	{redo,RURL}->
		?info({redirect,RURL}),
		%% we need to validate the redirect location
		%% to ensure we correctly handle local vs external
		%% redirects

		{Proto,Host,Port,Path}=xuri:parse(URL),

		MURL=
		case RURL of
		"http://"++_->
			RURL;
		
		P->
			makeurl(Proto,Host,Port,P)
		end,

		?info({newUrl,MURL}),

		session(HTTPRequest#httpRequest{url=MURL,
						redir_cnt=RedirCnt+1});
		
	Result->
		M(Result)
	end;

session(HTTPRequest=#httpRequest{url=URL,
				 redir_cnt=RedirCnt})->
	%% invokes a session as specified by HTTPRequest=#httpRequest
	%% get the #proxy{} information 

	#proxy{ip=Domain,port=Port}=server(HTTPRequest),

	%% create a fun that takes an IP address as input and pass
	%% to a handler

	F=fun(Z)->i_session(HTTPRequest,Z) end,

	I=(catch itcp_invoke:request(Domain,Port,F)),
	case I of
	{redo,RURL}->
		?info({redirect,RURL}),
		%% we need to validate the redirect location
		%% to ensure we correctly handle local vs external
		%% redirects

		{Proto,Host,Port,Path}=xuri:parse(URL),

		MURL=
		case RURL of
		"http://"++_->
			RURL;
		
		P->
			makeurl(Proto,Host,Port,P)
		end,

		?info({newUrl,MURL}),

		session(HTTPRequest#httpRequest{url=MURL,
						redir_cnt=RedirCnt+1});
		
	Result->
		Result
	end.

i_session(HTTPRequest,Socket)->
    
	HTTPHeader=httpHeader(	HTTPRequest#httpRequest.url,
				HTTPRequest#httpRequest.proxy,
				HTTPRequest#httpRequest.method,
				HTTPRequest#httpRequest.headers),


	%% Send msg and receive response
	ok=gen_tcp:send(Socket,HTTPHeader),
	Reply=
	case HTTPRequest#httpRequest.method of
	head->
		%% head request, we just return the HTTP response code
		case receive_header(Socket,[],true,?HTTP_TMOUT) of
		{ok,{R,H,Res}}->
                	{ok,H};
		Other->
			Other
		end;
	_->
		%% get request...
		greceive_header(Socket,[],true,?HTTP_TMOUT)
	end,
	gen_tcp:close(Socket),
	?dbug({i_session,{method,HTTPRequest#httpRequest.method},
			 {url,HTTPRequest#httpRequest.url},{reply,Reply}}),
	Reply.

%%% Extract proxy info for the connection

server(HTTPRequest)->
    case HTTPRequest#httpRequest.proxy of
	%% No proxy
	none->
	    Hostname=host(HTTPRequest#httpRequest.url),
	    Port=urlport(HTTPRequest#httpRequest.url),
	    #proxy{ip=Hostname,
		   port=Port};

	%% Use proxy
	Proxy->
	    Proxy
    end.

makeHeaders(Header) when list(Header)->
	lists:map(fun(Z)->makeHeaders(Z) end,Header);

makeHeaders({Key,Value}) when list(Key), list(Value)->
	Key ++ ": " ++ Value ++ "\r\n".

httpHeader(Url,Proxy,head,Headers)->
	httpHeader(Url,Proxy,"HEAD ",Headers);

httpHeader(Url,Proxy,get,Headers)->
	httpHeader(Url,Proxy,"GET ",Headers);

httpHeader(Url,Proxy,Method,Headers)->
    Port=integer_to_list(urlport(Url)),
    Host=host(Url),


    Dir=case Proxy of
	      none->

		  %% We need to extract host from Url

		  {_proto,_host,_port,D}=xuri:parse(Url),

	          D;
		 
	      Other->
		  dir(Url)
	  end,



    Hostname=lists:append(["http://",Host,":", Port, "/"]),
    Hostname2=lists:append([ Host,":",Port]),

    %% HTTP Header:
    Header=case Proxy of
		 none->
		     Method ++  Dir ++ " HTTP/1.1\r\n"
			 ++ "Host: " ++ Host ++ "\r\n";
		 AnOther->
		     Method ++  Hostname2 ++ Dir ++ " HTTP/1.1\r\n"
			 ++ "Host: " ++ Hostname2 ++ "\r\n"
	     end,
	Header ++ makeHeaders(Headers) ++ "\r\n".
    
	      
%%----------------------------------------------------------------------
%% Function: receive_header/4
%% Purpose:  Recursively receive header and response
%%           
%% Args:     Socket:       HTTP connection
%%           Bin:          data received so far
%%           KeepAlive:    boolean,does the server support keepalive mode?
%%           TimeoutDelay: max. delay between packets
%% Returns:  String
%%----------------------------------------------------------------------

receive_header(Socket,Bin,KeepAlive,TimeoutDelay)->
	receive
        {tcp,Socket,B}->
	    	%%B1= concat_binary([Bin,B]),
	    	B1= <<Bin,B>>,
            	case get_header(B1) of
			Z={ok,Reply={_,200,"OK"},Header,_}->
				{ok,Reply};

			Z={ok,Reply={_,R,_},Header,_} when R==301;
							      R==302;
							      R==303;
							      R==305;
							      R==307->
              			Size=content_length(Header),
               			case get_field(Header,"Location") of
                      		{true,URL1}->
					{ok,URL2,_}=regexp:gsub(URL1," ",""),
					{redirect,URL2};

				_->	%% try for lowercase (which is illegal!)
					case get_field(Header,"location") of
                                	{true,URL1}->
                                        	{ok,URL2,_}=regexp:gsub(URL1," ",""),
                                        	{redirect,URL2};

                       			_-> %% try for uppercase (also illegal!)
						
						case get_field(Header,"LOCATION") of
                                		{true,URL1}->
                                        		{ok,URL2,_}=regexp:gsub(URL1," ",""),
                                        		{redirect,URL2};
                       				_->
							{ok,Reply}
						end
					end
               			end;
				
                	Z={ok,Reply,Header,_}->
				{ok,Reply};

                	more->
                    		receive_header(Socket,B1,KeepAlive,TimeoutDelay)
	    	end;

        {tcp_closed,Socket}->
            {error,{http,socket_closed_in_header}};

        {tcp_error,Socket,Reason}->
            {error,{http,{tcp_error,Reason}}};

	Other->
	   %% {error,{socket,Other}}
	    {error,{http,{socket,other}}}

    after TimeoutDelay->
	    {error,{http,{timeout,TimeoutDelay}}}
    end.

%% use this for GET requests 

greceive_header(Socket,Bin,KeepAlive,TimeoutDelay)->
	receive
        {tcp,Socket,B} ->
		%%B1=concat_binary([Bin,B]),
	    	B1= <<Bin,B>>,
		case get_header(B1) of
		{ok,Reply,Header,BT}->
			Size=content_length(Header),
			?dbug({headerSize,Size}),
			case get_field(Header,"Location") of
			{true,URL1} ->
                            %% If it's redo we still have to get the body
                            %% to flush the socket
				case receive_body(Size,BT,Socket,KeepAlive,TimeoutDelay) of
                                {ok,Body}->
					{redo,URL1};
				Error->
					Error
				end;
                        _->
				case receive_body(Size,BT,Socket,KeepAlive,TimeoutDelay) of
                                {ok,Body}->
					{ok,{Reply,Header,binary_to_list(Body)}};
				Error->
					Error
				end
			end;
		more->
			greceive_header(Socket,B1,KeepAlive,TimeoutDelay)
		end;
	{tcp_closed,Socket}->
		{error,socket_closed_in_header};
	{tcp_error,Socket, Reason}->
		{error,Reason};
	Other->
		{error,{socket,Other}}
	after TimeoutDelay->
		{error,{http_timeout,TimeoutDelay}}
    	end.


receive_body(Size={length,S},Bin,Socket,KeepAlive,TimeoutDelay) when size(Bin) < S->
	receive
	{tcp,Socket,B}->
	    	B1= <<Bin,B>>,
		%%B1=concat_binary([Bin,B]),
		receive_body(Size,B1,Socket,KeepAlive,TimeoutDelay);
	{tcp_closed,Socket}->
		{ok,Bin};
	{tcp_error,Socket,What}->
		{error,{socket,What}};
        Other->
            {error,{socket, Other}}
    	after TimeoutDelay->
		{error,{http_timeout,TimeoutDelay}}
    end;


receive_body(Size=not_known,Bin,Socket,KeepAlive,TimeoutDelay)->
	?dbug({loopingUnknownSize,{rcvd,size(Bin)}}),
	
	receive
	{tcp,Socket,B}->
	    	B1= <<Bin,B>>,
		%%B1=concat_binary([Bin,B]),
		receive_body(Size,B1,Socket,KeepAlive,TimeoutDelay);
	{tcp_closed,Socket}->
		?dbug({Socket,tcp_closed}),
		{ok,Bin};
	{tcp_error,Socket,What}->
		?dbug({Socket,tcp_error,What}),
		{error,{socket,What}};
        Other->
		?dbug({Socket,other,Other}),
            	{error,{socket, Other}}
    	after TimeoutDelay->
		%% {error,{http_timeout,TimeoutDelay}},
		?warn({httpTimeout,TimeoutDelay}),
		{ok,Bin}
    end;

receive_body(Size,Bin,Socket,KeepAlive,TimeoutDelay)->
	%% should we close the socket here ???
	{ok,Bin}.

get_header(B)->
	L=binary_to_list(B),
	case split_header(L,[]) of
		{ReplyHeader,Rest}->
            		{Reply,Header}=parse_reply(ReplyHeader),
			H=parse_header(Header),
            		{ok,Reply,H,list_to_binary(Rest)};
        	fail->
            		more
	end.

host(Url)->
    Result=string:tokens(Url,":/"),
    Hostname=lists:nth(2,Result).

%% TODO: Add last '/' if it exists
dir(Url)->
    Result=string:tokens(Url,"/"),
    Result2=lists:map(fun(X)-> lists:append("/",X) end,lists:nthtail(2, Result)),
    lists:append(Result2).

%%----------------------------------------------------------------------
%% Function: urlport/1
%% Purpose:  Return the port used given an url
%% Args:     Url: string
%% Returns:  Portnumber: Integer
%%----------------------------------------------------------------------
urlport(Url)->
    Result=string:tokens(Url,":/"),
    case length(Result) of
	Len when Len > 2->
	    portnumber(lists:nth(3,Result));
	Len->
	    80
    end.
portnumber(String)->
    case catch list_to_integer(String) of
	{'EXIT',Reason}->
	    80;
	Number->
	    Number
    end.


content_length(Header)->
    case get_field(Header,"Content-Length") of
        {true,Str}->
            {length,list_to_integer(Str)};
        {false,_}->
            not_known
    end.

get_field([{K,V}|T],K)-> {true,V};
get_field([_|T],K)-> get_field(T,K);
get_field([],_)-> {false,false}.
 % It looks like parse_header/2 includes split header ???? - roland
parse_header([$\r,$\n | T],Info)-> header_end(T, Info);
parse_header([$\n | T],Info)-> header_end(T,Info);
parse_header(Cs,Info)-> header_line(Cs,[], Info).


parse_header(T)->
    {_,P}=parse_header(T,[]),
    P.


header_line([$\r,$\n | T],Acc, Info)->
    parse_header(T,[split_info(lists:reverse(Acc)) | Info]);
header_line([$\n | T],Acc,Info)->
    parse_header(T,[split_info(lists:reverse(Acc)) | Info]);
header_line([C | Cs],Acc,Info)->
    header_line(Cs,[C | Acc],Info);
header_line([],Acc,Info)->
    header_end([],[split_info(lists:reverse(Acc)) | Info]).

header_end([$\r,$\n | T],Info)-> header_end(T, Info);
header_end([$\n | T],Info)-> header_end(T,Info);
header_end(T,Info)-> {T,Info}.


split_header([$\r,$\n,$\r,$\n|T],L)-> {lists:reverse(L), T};
split_header([$\n,$\n|T],L)-> {lists:reverse(L), T};
split_header([H|T],L)-> split_header(T,[H|L]);
split_header([],L)-> fail.

split_info(String)->
    case string:chr(String,$:) of
        0-> {"Parse-Error",trim(String)};
        Ix->
            {trim(string:substr(String,1,Ix-1)),
	    trim(string:substr(String, Ix+1, length(String)))}
    end.

trim(String)->
    lists:reverse(strip(lists:reverse(strip(String)))).

parse_reply(R0)->
	case (catch get_until(R0,eol,[])) of
	{'EXIT',Reason}->
		throw(?HTTPERROR);

	Y={ZZ,Header}->
		case regexp:split(ZZ," ") of
		{ok,[HTTP,CODE,COMM|Rest]}->
			case (catch list_to_integer(CODE)) of
			{'EXIT',Reason}->
				?warn({parse_reply,{httpError,Y}}),
				throw(?HTTPERROR);
			ICODE->
				{{HTTP,ICODE,COMM},Header}
			end;

		{ok,[HTTP,CODE]}->
			case (catch list_to_integer(CODE)) of
			{'EXIT',Reason}->
				?warn({parse_reply,{httpError,Y}}),
				throw(?HTTPERROR);
			ICODE->
				{{HTTP,ICODE,""},Header}
			end;
		Other->
			throw(?HTTPERROR)
		end
	end.

get_until([R|Rs],R,L)->
    	{lists:reverse(L),Rs};
get_until([$\r,$\n|Rs],eol,L)->
	{lists:reverse(L),Rs};
get_until([$\n|Rs],eol,L)->
	{lists:reverse(L),Rs};
get_until([],eol,L)->
	{lists:reverse(L),[]};
get_until([R|Rs],P,L)->
    	get_until(Rs,P,[R|L]).
	

strip([$   | Cs])-> strip(Cs);
strip([$\t | Cs])-> strip(Cs);
strip([$\r | Cs])-> strip(Cs);
strip([$\n | Cs])-> strip(Cs);
strip(Cs)-> Cs.

%% Topic analysis

f([T1,T2,?WEB])->
	[T1,?WEB];

f(Ts)->
	Ts.

heirarchy(Topic)->
	%%H=f(heirarchy(t_parent(Topic),[Topic])),
	H=heirarchy(t_parent(Topic),[Topic]),
	{?FACETMAP,lists:map(fun(Z)->element(2,Z) end,H)}.

heirarchy(?FROOT,C)->
	C;

heirarchy(?NULL,C)->
	C;

heirarchy(Topic,Topics)->
	heirarchy(t_parent(Topic),lists:append(Topics,[Topic])).

t_parent(Topic) when	Topic==?BADARG;
			Topic==?OTHERERROR->
	?ERROR;

t_parent(Topic)	when	Topic==?DNSERROR;
			Topic==?NOARECS;
			Topic==?TCPERROR;
			Topic==?ECONNREFUSED;
			Topic==?NOHTTP->
	?NOWEB;

t_parent(Topic) when	Topic==?HTTPCONTINUE;
			Topic==?HTTPPAGE;
			Topic==?HTTPREDIRECT;
			Topic==?HTTPPAGEERROR;
			Topic==?HTTPSERVERERROR->
	?WEB;

t_parent(Topic)	when	Topic==?WEB;
			Topic==?NOWEB;
			Topic==?ERROR->
	?FROOT;

t_parent({?FACETMAP,"http1"++Rest})->
	?HTTPCONTINUE;

t_parent({?FACETMAP,"http2"++Rest})->
	?HTTPPAGE;

t_parent({?FACETMAP,"http4"++Rest})->
	?HTTPPAGEERROR;

t_parent({?FACETMAP,"http5"++Rest})->
	?HTTPSERVERERROR;

t_parent({?FACETMAP,"redirect"++Rest})->
	?HTTPREDIRECT;

t_parent(?HTTPERROR)->
	?WEB;

t_parent(Other)->
	?warn({no_parent,Other}),
	?NULL.

