-define(UA,	{"User_Agent","Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0; .NET CLR 1.0.3705)"}).

-record(httpRequest, {
	  url,
	  method,
	  proxy=none,
	  headers=[?UA],
	  http_tmout=10000,
	  module=undefined,
	  redir_cnt=0
	 }).

-record(proxy, {
          ip,
          port
          }).

