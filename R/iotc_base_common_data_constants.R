CL_CACHE = new.env(hash = TRUE)

### Columns common to all data sets

#'The year column constant
#'@export
YEAR                    = "YEAR"

#'The quarter column constant
#'@export
QUARTER                 = "QUARTER"

#'The month (start) column constant
#'@export
MONTH_START             = "MONTH_START"

#'The month (end) column constant
#'@export
MONTH_END               = "MONTH_END"

#'The flag code column constant
#'@export
FLAG_CODE               = "FLAG_CODE"

#'The fleet code column constant
#'@export
FLEET_CODE              = "FLEET_CODE"

#'The fleet column constant
#'@export
FLEET                   = "FLEET"

#'The gear code column constant
#'@export
GEAR_CODE               = "GEAR_CODE"

#'The gear column constant
#'@export
GEAR                    = "GEAR"

#'The fishery code column constant
#'@export
FISHERY_CODE            = "FISHERY_CODE"

#'The fishery column constant
#'@export
FISHERY                 = "FISHERY"

#'The fishery group code column constant
#'@export
FISHERY_GROUP_CODE      = "FISHERY_GROUP_CODE"

#'The fishery group column constant
#'@export
FISHERY_GROUP           = "FISHERY_GROUP"

#'The fishery type code column constant
#'@export
FISHERY_TYPE_CODE       = "FISHERY_TYPE_CODE"

#'The fishery type column constant
#'@export
FISHERY_TYPE            = "FISHERY_TYPE"

#'The species code column constant
#'@export
SPECIES_CODE            = "SPECIES_CODE"

#'The species column constant
#'@export
SPECIES                 = "SPECIES"

#'The species category code column constant
#'@export
SPECIES_CATEGORY_CODE   = "SPECIES_CATEGORY_CODE"

#'The species category column constant
#'@export
SPECIES_CATEGORY        = "SPECIES_CATEGORY"

#'The species group column constant
#'@export
SPECIES_GROUP           = "SPECIES_GROUP"

#'The species group code column constant
#'@export
SPECIES_GROUP_CODE      = "SPECIES_GROUP_CODE"

#'The species group column constant
#'@export
SPECIES_GROUP           = "SPECIES_GROUP"

#'The working party code column constant
#'@export
SPECIES_WP_CODE         = "SPECIES_WP_CODE"

#'The working party column constant
#'@export
SPECIES_WP              = "SPECIES_WP"

#'The 'is IOTC species' column constant
#'@export
IS_IOTC_SPECIES         = "IS_IOTC_SPECIES"

#'The 'is species aggregate' column constant
#'@export
IS_SPECIES_AGGREGATE    = "IS_SPECIES_AGGREGATE"

#'The 'is species of special interest' column constant
#'@export
IS_SSI                  = "IS_SSI"

#'The 'IUCN status code' column constant
#'@export
IUCN_STATUS_CODE        = "IUCN_STATUS_CODE"

#'The 'IUCN status' column constant
#'@export
IUCN_STATUS             = "IUCN_STATUS"

#'The fishing ground column constant
#'@export
FISHING_GROUND          = "FISHING_GROUND"

#'The fishing ground code column constant
#'@export
FISHING_GROUND_CODE     = "FISHING_GROUND_CODE"

#'The raising column constant
#'@export
RAISING                 = "RAISING"

#'The raise code column constant
#'@export
RAISE_CODE              = "RAISE_CODE"

### Columns specific to nominal catch and geo-referenced catch data sets

#'The catch column constant
#'@export
CATCH                   = "CATCH"

#'The catch unit code column constant
#'@export
CATCH_UNIT_CODE         = "CATCH_UNIT_CODE"

### Columns specific to nominal catch (raised) data set

#'The catch in numbers column constant
#'@export
CATCH_IN_NUMBERS        = "CATCH_IN_NUMBERS"

#'The fate column constant
#'@export
FATE                    = "FATE"

#'The fate code column constant
#'@export
FATE_CODE               = "FATE_CODE"

#'The fate type column constant
#'@export
FATE_TYPE               = "FATE_TYPE"

#'The fate type code column constant
#'@export
FATE_TYPE_CODE          = "FATE_TYPE_CODE"

### Columns specific to catch-and-effort data sets

#'The effort school type code column constant
#'@export
EFFORT_SCHOOL_TYPE_CODE = "EFFORT_SCHOOL_TYPE_CODE"

#'The catch school type code column constant
#'@export
CATCH_SCHOOL_TYPE_CODE  = "CATCH_SCHOOL_TYPE_CODE"

#'The effort column constant
#'@export
EFFORT                  = "EFFORT"

#'The effort unit code column constant
#'@export
EFFORT_UNIT_CODE        = "EFFORT_UNIT_CODE"

# Columns specific to size-Frequency data sets only

#'The school type code column constant
#'@export
SCHOOL_TYPE_CODE        = "SCHOOL_TYPE_CODE"

#'The measure type codecolumn constant
#'@export
MEASURE_TYPE 		        = "MEASURE_TYPE"


#'The measure type code column constant
#'@export
MEASURE_TYPE_CODE 		  = "MEASURE_TYPE_CODE"

#'The measure unit code column constant
#'@export
MEASURE_UNIT_CODE 		  = "MEASURE_UNIT_CODE"

#'The length measure type code column constant
#'@export
LENGTH_MEASURE_TYPE_CODE= "LENGTH_MEASURE_TYPE_CODE"

#'The length measure unit code column constant
#'@export
LENGTH_MEASURE_UNIT_CODE= "LENGTH_MEASURE_UNIT_CODE"

#'The weight measure type code column constant
#'@export
WEIGHT_MEASURE_TYPE_CODE= "WEIGHT_MEASURE_TYPE_CODE"

#'The weight measure unit code column constant
#'@export
WEIGHT_MEASURE_UNIT_CODE= "WEIGHT_MEASURE_UNIT_CODE"

#'The sex code column constant
#'@export
SEX_CODE 				        = "SEX_CODE"

#'The sample size column constant
#'@export
SAMPLE_SIZE 			      = "SAMPLE_SIZE"

#'The 'first class low' column constant
#'@export
FIRST_CLASS_LOW 		    = "FIRST_CLASS_LOW"

#'The size class column constant
#'@export
SIZE_INTERVAL 			    = "SIZE_INTERVAL"

#'The class low column constant
#'@export
CLASS_LOW 				      = "CLASS_LOW"

#'The class high column constant
#'@export
CLASS_HIGH 				      = "CLASS_HIGH"

#'The fish count column constant
#'@export
FISH_COUNT 				      = "FISH_COUNT"

#'The tot num fish column constant
#'@export
TOT_NUM_FISH 			      = "TOT_NUM_FISH"

#'The tot kg fish column constant
#'@export
TOT_KG_FISH 			      = "TOT_KG_FISH"

### Misc

#'The 'Quality' column constant
#'@export
QUALITY 			          = "QUALITY"

#'The 'Quality code' column constant
#'@export
QUALITY_CODE            = "QUALITY_CODE"

### Columns specific to ROS interaction data

#'The num. interactions column constant
#'@export
NUM_INTERACTIONS        = "NUM_INTERACTIONS"

#'The condition column constant
#'@export
CONDITION               = "CONDITION"

#'The condition code column constant
#'@export
CONDITION_CODE          = "CONDITION_CODE"

#'The condition type column constant
#'@export
CONDITION_TYPE          = "CONDITION_TYPE"

#'The condition type code column constant
#'@export
CONDITION_TYPE_CODE     = "CONDITION_TYPE_CODE"

# ROS raw sets

#'The event type code column constant
#'@export
EVENT_TYPE_CODE     = "EVENT_TYPE_CODE"

#'The start time column constant
#'@export
START_TIME     = "START_TIME"

#'The start lon column constant
#'@export
START_LON = "START_LON"

#'The start lat column constant
#'@export
START_LAT = "START_LAT"

#'The end time column constant
#'@export
END_TIME     = "END_TIME"

#'The end lon column constant
#'@export
END_LON = "END_LON"

#'The end lat column constant
#'@export
END_LAT = "END_LAT"
