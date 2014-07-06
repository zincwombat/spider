-include("nstatus.hrl").

i_schemaLoc(?NSTATUS_V1_URI)->
        {schemaLoc,nstatus_v1_schema};

i_schemaLoc(Other)->
	?error({no_schema_location,Other}).
