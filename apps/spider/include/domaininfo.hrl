-record(domain, {
                        fqdn,           %% fully qualified domain name
                        space,          %% name space
                        parent,         %% parent name
                        name,           %% the name (less any extensions)
			rname,		%% registered domain
                        class,          %% cctld,gtld etc
                        subdomains      %% list of all subdomains}).
}).

-record(space,	{
			space,
			type,
			numSegs
}).

-record(dnsinfo,{
			name,
			arecs,
			nsrecs,
			mxrecs
}).
