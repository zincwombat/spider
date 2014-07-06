-module(facet).
-define(TRACE_LEVEL,?TRACE_INFO).
-include_lib("esn_kernel/include/debug.hrl").

-include("http.hrl").
-include("agent.hrl").
-include("urn_domain.hrl").
-include("filter.hrl").

-sysloglevel(?TRACE_WARN).

-export([invoke/2]).

invoke(Host,Type)->
	%% i_invoke(Host,Type,ns2:authNs(Host)).
	i_invoke(Host,Type).

i_invoke(Host,Type,{ns,{error,E}})->
	{error,{Host,{dns,E}}};

i_invoke(Host,Type,{ns,NS})->
	i_invoke(Host,Type).

i_invoke(Host,mail)->
	imail:mcheck(Host);

i_invoke(Host,dns)->
	dnutils:dnsCheck(Host);

i_invoke(Host,web)->
	case Host of
	"http://" ++ Rest->
		{Rest,facetmap_web_v1:xclassify(ihttpc:xhead(Host))};

	"https://" ++ Rest->
		{Rest,facetmap_web_v1:xclassify(ihttpc:xhead(Host))};

	_->
		i_invoke("http://" ++ Host,web)
	end;

i_invoke(Host,spider)->
	ihttpc:hget(Host);

i_invoke(Host,Unknown)->
	{error,{unknown_facet,Unknown}}.


