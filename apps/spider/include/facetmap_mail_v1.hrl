-define(FACETMAP,		"urn:facetmap:mail_v1").

-define(DNSERROR,               {?FACETMAP,"dnsError"}).
-define(NOARECS,                {?FACETMAP,"noARecs"}).
-define(NOMXRECS,               {?FACETMAP,"noMXRecs"}).
-define(NODIRECTSMTP,           {?FACETMAP,"noDirectSmtp"}).
-define(TCPERROR,               {?FACETMAP,"tcpError"}).
-define(ECONNREFUSED,           {?FACETMAP,"econnrefused"}).
-define(BADARG,                 {?FACETMAP,"badarg"}).
-define(OTHERERROR,             {?FACETMAP,"otherError"}).

-define(FROOT,                  {?FACETMAP,"root"}).
-define(NULL,                   {?FACETMAP,"null"}).

%% top level TOPICS

-define(MAIL,			{?FACETMAP,"mail"}).
-define(NOMAIL,			{?FACETMAP,"nomail"}).
-define(ERROR,                  {?FACETMAP,"error"}).
-define(NOSMTP,			{?FACETMAP,"nosmtp"}).
-define(SMTP,			{?FACETMAP,"smtp"}).
-define(SMTPERROR,		{?FACETMAP,"smtpError"}).

-define(MKTOPIC(Code),          {?FACETMAP,"smtp"++integer_to_list(Code)}).


