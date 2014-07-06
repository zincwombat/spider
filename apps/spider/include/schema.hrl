-define(UNDEF_ATTR,	'__undefined_attribute__').
-define(NOTFIXED_ATTR,	'__attribute_not_fixed__').
-define(NODEFAULT_ATTR,	'__attribute_no_default__').
-define(ANY_CONTENT,	'__any_content__').

-record(schema,         {name,
			 xmlns,
                         attributes=[],
			 model,
			 constructor,
			 attributeConstructor,
                         content=[]}).

-record(i_schema,	{model,
			 attributes=[],
			 content}).

-record(attrSchema,	{name,
			 value,
			 type,
			 use,
			 default=?NODEFAULT_ATTR,
			 fixed=?NOTFIXED_ATTR}).

-record(elementSchema,	{name,
			 nsinfo,
			 minoccurs,
			 maxoccurs,
			 actual}).

-record(xmlValue,	{name,
			 attrs=[],
			 value}).

-record(version,	{version}).

