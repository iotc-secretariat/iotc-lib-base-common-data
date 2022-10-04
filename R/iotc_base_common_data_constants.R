CL_CACHE = new.env(hash = TRUE)

### Columns common to all data sets

#'The year column constant
#'@export
C_YEAR                    = "YEAR"

#'The quarter column constant
#'@export
C_QUARTER                 = "QUARTER"

#'The month (start) column constant
#'@export
C_MONTH_START             = "MONTH_START"

#'The month (end) column constant
#'@export
C_MONTH_END               = "MONTH_END"

#'The flag code column constant
#'@export
C_FLAG_CODE               = "FLAG_CODE"

#'The fleet code column constant
#'@export
C_FLEET_CODE              = "FLEET_CODE"

#'The fleet column constant
#'@export
C_FLEET                   = "FLEET"

#'The gear code column constant
#'@export
C_GEAR_CODE               = "GEAR_CODE"

#'The gear column constant
#'@export
C_GEAR                    = "GEAR"

#'The fishery code column constant
#'@export
C_FISHERY_CODE            = "FISHERY_CODE"

#'The fishery column constant
#'@export
C_FISHERY                 = "FISHERY"

#'The fishery group code column constant
#'@export
C_FISHERY_GROUP_CODE      = "FISHERY_GROUP_CODE"

#'The fishery group column constant
#'@export
C_FISHERY_GROUP           = "FISHERY_GROUP"

#'The fishery type code column constant
#'@export
C_FISHERY_TYPE_CODE       = "FISHERY_TYPE_CODE"

#'The fishery type column constant
#'@export
C_FISHERY_TYPE            = "FISHERY_TYPE"

#'The species code column constant
#'@export
C_SPECIES_CODE            = "SPECIES_CODE"

#'The species column constant
#'@export
C_SPECIES                 = "SPECIES"

#'The species category code column constant
#'@export
C_SPECIES_CATEGORY_CODE   = "SPECIES_CATEGORY_CODE"

#'The species category column constant
#'@export
C_SPECIES_CATEGORY        = "SPECIES_CATEGORY"

#'The species group column constant
#'@export
C_SPECIES_GROUP           = "SPECIES_GROUP"

#'The species group code column constant
#'@export
C_SPECIES_GROUP_CODE      = "SPECIES_GROUP_CODE"

#'The species group column constant
#'@export
C_SPECIES_GROUP           = "SPECIES_GROUP"

#'The working party code column constant
#'@export
C_SPECIES_WP_CODE         = "SPECIES_WP_CODE"

#'The working party column constant
#'@export
C_SPECIES_WP              = "SPECIES_WP"

#'The 'is IOTC species' column constant
#'@export
C_IS_IOTC_SPECIES         = "IS_IOTC_SPECIES"

#'The 'is species aggregate' column constant
#'@export
C_IS_SPECIES_AGGREGATE    = "IS_SPECIES_AGGREGATE"

#'The 'is species of special interest' column constant
#'@export
C_IS_SSI                  = "IS_SSI"

#'The 'IUCN status code' column constant
#'@export
C_IUCN_STATUS_CODE        = "IUCN_STATUS_CODE"

#'The 'IUCN status' column constant
#'@export
C_IUCN_STATUS             = "IUCN_STATUS"

#'The fishing ground column constant
#'@export
C_FISHING_GROUND          = "FISHING_GROUND"

#'The fishing ground code column constant
#'@export
C_FISHING_GROUND_CODE     = "FISHING_GROUND_CODE"

#'The raising column constant
#'@export
C_RAISING                 = "RAISING"

#'The raise code column constant
#'@export
C_RAISE_CODE              = "RAISE_CODE"

### Columns specific to nominal catch and geo-referenced catch data sets

#'The catch column constant
#'@export
C_CATCH                   = "CATCH"

#'The catch unit code column constant
#'@export
C_CATCH_UNIT_CODE         = "CATCH_UNIT_CODE"

### Columns specific to nominal catch (raised) data set

#'The catch in numbers column constant
#'@export
C_CATCH_IN_NUMBERS        = "CATCH_IN_NUMBERS"

#'The fate column constant
#'@export
C_FATE                    = "FATE"

#'The fate code column constant
#'@export
C_FATE_CODE               = "FATE_CODE"

#'The fate type column constant
#'@export
C_FATE_TYPE               = "FATE_TYPE"

#'The fate type code column constant
#'@export
C_FATE_TYPE_CODE          = "FATE_TYPE_CODE"

### Columns specific to catch-and-effort data sets

#'The effort school type code column constant
#'@export
C_EFFORT_SCHOOL_TYPE_CODE = "EFFORT_SCHOOL_TYPE_CODE"

#'The catch school type code column constant
#'@export
C_CATCH_SCHOOL_TYPE_CODE  = "CATCH_SCHOOL_TYPE_CODE"

#'The effort column constant
#'@export
C_EFFORT                  = "EFFORT"

#'The effort unit code column constant
#'@export
C_EFFORT_UNIT_CODE        = "EFFORT_UNIT_CODE"

# Columns specific to size-Frequency data sets only

#'The school type code column constant
#'@export
C_SCHOOL_TYPE_CODE        = "SCHOOL_TYPE_CODE"

#'The measure type codecolumn constant
#'@export
C_MEASURE_TYPE 		        = "MEASURE_TYPE"


#'The measure type code column constant
#'@export
C_MEASURE_TYPE_CODE 		  = "MEASURE_TYPE_CODE"

#'The measure unit code column constant
#'@export
C_MEASURE_UNIT_CODE 		  = "MEASURE_UNIT_CODE"

#'The length measure type code column constant
#'@export
C_LENGTH_MEASURE_TYPE_CODE= "LENGTH_MEASURE_TYPE_CODE"

#'The length measure unit code column constant
#'@export
C_LENGTH_MEASURE_UNIT_CODE= "LENGTH_MEASURE_UNIT_CODE"

#'The weight measure type code column constant
#'@export
C_WEIGHT_MEASURE_TYPE_CODE= "WEIGHT_MEASURE_TYPE_CODE"

#'The weight measure unit code column constant
#'@export
C_WEIGHT_MEASURE_UNIT_CODE= "WEIGHT_MEASURE_UNIT_CODE"

#'The sex code column constant
#'@export
C_SEX_CODE 				        = "SEX_CODE"

#'The sample size column constant
#'@export
C_SAMPLE_SIZE 			      = "SAMPLE_SIZE"

#'The 'first class low' column constant
#'@export
C_FIRST_CLASS_LOW 		    = "FIRST_CLASS_LOW"

#'The size class column constant
#'@export
C_SIZE_INTERVAL 			    = "SIZE_INTERVAL"

#'The class low column constant
#'@export
C_CLASS_LOW 				      = "CLASS_LOW"

#'The class high column constant
#'@export
C_CLASS_HIGH 				      = "CLASS_HIGH"

#'The fish count column constant
#'@export
C_FISH_COUNT 				      = "FISH_COUNT"

#'The tot num fish column constant
#'@export
C_TOT_NUM_FISH 			      = "TOT_NUM_FISH"

#'The tot kg fish column constant
#'@export
C_TOT_KG_FISH 			      = "TOT_KG_FISH"

### Misc

#'The 'Quality' column constant
#'@export
C_QUALITY 			          = "QUALITY"

#'The 'Quality code' column constant
#'@export
C_QUALITY_CODE            = "QUALITY_CODE"

### Columns specific to ROS interaction data

#'The num. interactions column constant
#'@export
C_NUM_INTERACTIONS        = "NUM_INTERACTIONS"

#'The condition column constant
#'@export
C_CONDITION               = "CONDITION"

#'The condition code column constant
#'@export
C_CONDITION_CODE          = "CONDITION_CODE"

#'The condition type column constant
#'@export
C_CONDITION_TYPE          = "CONDITION_TYPE"

#'The condition type code column constant
#'@export
C_CONDITION_TYPE_CODE     = "CONDITION_TYPE_CODE"

# ROS raw sets

#'The event type code column constant
#'@export
C_EVENT_TYPE_CODE     = "EVENT_TYPE_CODE"

#'The start time column constant
#'@export
C_START_TIME     = "START_TIME"

#'The start lon column constant
#'@export
C_START_LON = "START_LON"

#'The start lat column constant
#'@export
C_START_LAT = "START_LAT"

#'The end time column constant
#'@export
C_END_TIME     = "END_TIME"

#'The end lon column constant
#'@export
C_END_LON = "END_LON"

#'The end lat column constant
#'@export
C_END_LAT = "END_LAT"
