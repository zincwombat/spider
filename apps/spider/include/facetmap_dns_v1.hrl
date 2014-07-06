-define(FACETMAP,	"urn:facetmap:dns_v1").
-define(MKTOPIC(T),	{?FACETMAP,T}).

-define(F_DNSV1_NXDOMAIN,	?MKTOPIC("nxdomain")).
-define(F_DNSV1_SERVFAIL,	?MKTOPIC("servfail")).
-define(F_DNSV1_TIMEOUT,	?MKTOPIC("timeout")).
-define(F_DNSV1_OTHERDNS,	?MKTOPIC("otherError")).
-define(F_DNSV1_ISZONE,		?MKTOPIC("isZone")).
-define(F_DNSV1_ISNOTZONE,	?MKTOPIC("isNotZone")).
-define(F_DNSV1_ISSUBDOMAIN,	?MKTOPIC("isSubDomain")).
-define(F_DNSV1_ISWILDCARD,	?MKTOPIC("isWildCard")).
-define(F_DNSV1_DNSERROR,	?MKTOPIC("dnsError")).
-define(F_DNSV1_DNSFAIL,	?MKTOPIC("dnsFail")).
-define(F_DNSV1_DELEGATED,	?MKTOPIC("delegated")).
-define(F_DNSV1_ERROR,		?MKTOPIC("error")).
-define(F_DNSV1_FORMATERROR,	?MKTOPIC("formatError")).
-define(F_DNSV1_FROOT,		?MKTOPIC("root")).
-define(F_DNSV1_NULL,		?MKTOPIC("null")).

-define(ERRSTR(X),		list_to_atom(element(2,X))).


