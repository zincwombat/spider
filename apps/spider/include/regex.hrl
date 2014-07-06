-define(INIT_REGEX_LIST, [href,url,email,title,domain,meta_keyword,copyright,phone,htmltag,meta_description,company_nr]).

-define(re_title,	"<title>([^<]*)").
-define(re_url,		"(http[s]?://)[a-z0-9_-]+([.][a-z0-9_-]+)*(/[a-z0-9_-]+([.][0-9a-z_-]+)?)*(/|\\?[a-z0-9_]*=[a-z0-9_]*(&[a-z0-9_]*=[a-z0-9_]*)*)?").
-define(re_email,	"[A-Z0-9._%+-]+@[A-Z0-9.-]+[.][A-Z]{2,4}").
-define(re_domain,	"([a-zA-Z0-9]([a-zA-Z0-9\-]{0,61}[a-zA-Z0-9])?[.])+([a-zA-Z]{2,6})").
-define(re_href,	"href=[\\\"\\\']((http[s]?://|[.]/|/)?[a-z0-9_]+([.][a-z0-9_]+)*(/[a-z0-9_]+([.][0-9a-z_]+)?)*(/|\\?[a-z0-9_]*=[a-z0-9_]*(&[a-z0-9_]*=[a-z0-9_]*)*)?)[\\\"\\\']").
-define(re_copyright,	"(copyright|&copy;|&#169;|copyright[ ]*&copy;)[ ]*(.{90})").
-define(re_phone,	[92,$(,${,$0,$,,$1,$},$(,$(,$0,$|,92,$+,$6,$1,$),$(,$2,$|,$4,$|,$3,$|,$7,$|,$8,$),$),${,$0,$,,$1,$},92,$),${,$0,$,,$1,$},$(,32,$|,$-,$),${,$0,$,,$1,$},$[,$0,$-,$9,$],${,$2,$},$(,32,$|,$-,$),${,$0,$,,$1,$},$[,$0,$-,$9,$],${,$2,$},$(,32,$|,$-,$),${,$0,$,,$1,$},$[,$0,$-,$9,$],${,$1,$},$(,32,$|,$-,$),${,$0,$,,$1,$},$[,$0,$-,$9,$],${,$3,$}]).

-define(re_htmltag,	"<[^>]+>"). 
-define(re_meta_kw,	"<meta name=[\"']keywords[\"'][ ]*content=[\"']([^>]+)[\"'][ ]*/>"). 
-define(re_meta_description,	"<meta name=[\"']description[\"'][ ]*content=[\"']([^>]+)[\"'][ ]*/>"). 
-define(re_company_nr,	"(A[.]{0,1}C[.]{0,1}N[.]{0,1}|A[.]{0,1}B[.]{0,1}N[.]{0,1}|A[.]{0,1}B[.]{0,1}R[.]{0,1}N[.]{0,1})[:]*[ ]*([0-9$ ]+)").


-define(DEFAULT_MAX_MATCHES,	200).
