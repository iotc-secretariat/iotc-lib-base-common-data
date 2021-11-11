#'Extracts all details of the \code{[meta].WORKING_PARTIES} table as available on \code{IOTDB}
#'@param connection A connection to the \code{\link{IOTDB}}
#'@return a data frame with multiple rows (one for each working party) and the required columns to model the structure of the table
#'@export
all_wps = function(connection = DB_IOTDB()) {
  return(all_meta("WORKING_PARTIES", connection))
}

#'Extracts all details of the \code{[meta].SPECIES_GROUPS} table as available on \code{IOTDB}
#'@param connection A connection to the \code{\link{IOTDB}}
#'@return a data frame with multiple rows (one for each species group) and the required columns to model the structure of the table
#'@export
all_species_groups = function(connection = DB_IOTDB()) {
  return(all_meta("SPECIES_GROUPS", connection))
}

#'Extracts all details of the \code{[meta].SPECIES_CATEGORIES} table as available on \code{IOTDB}
#'@param connection A connection to the \code{\link{IOTDB}}
#'@return a data frame with multiple rows (one for each species category) and the required columns to model the structure of the table
#'@export
all_species_categories = function(connection = DB_IOTDB()) {
  return(all_meta("SPECIES_CATEGORIES", connection))
}

#'Extracts all details of the \code{[meta].SPECIES} table as available on \code{IOTDB}
#'@param connection A connection to the \code{\link{IOTDB}}
#'@return a data frame with multiple rows (one for each species) and the required columns to model the structure of the table
#'@export
all_species = function(connection = DB_IOTDB()) {
  return(all_meta("SPECIES", connection))
}

#'Factorizes the species working party codes in a data frame
#'@param data The data frame whose \code{SPECIES_WP_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{SPECIES_WP_CODE} factorized according to the default sequence of species group codes
#'@export
factorize_species_wps = function(data, connection = DB_IOTDB()) {
  if(!(SPECIES_WP_CODE %in% colnames(data))) {
    return (data)
  }

  WPS = all_wps(connection)[order(+SORT)]

  levels = as.character(WPS$CODE)
  labels = as.character(WPS$NAME_EN)

  data$SPECIES_WP = data$SPECIES_WP_CODE

  data$SPECIES_WP =
    factor(
      data$SPECIES_WP,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$SPECIES_WP_CODE =
    factor(
      data$SPECIES_WP_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$SPECIES_WP),])
}

#'Factorizes the species group codes in a data frame
#'@param data The data frame whose \code{SPECIES_GROUP_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{SPECIES_GROUP_CODE} factorized according to the default sequence of species group codes
#'@export
factorize_species_groups = function(data, connection = DB_IOTDB()) {
  if(!(SPECIES_GROUP_CODE %in% colnames(data))) {
    return (data)
  }

  SG = all_species_groups(connection)[order(+SORT)]

  levels = as.character(SG$CODE)
  labels = as.character(SG$NAME_EN)

  data$SPECIES_GROUP = data$SPECIES_GROUP_CODE

  data$SPECIES_GROUP =
    factor(
      data$SPECIES_GROUP,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$SPECIES_GROUP_CODE =
    factor(
      data$SPECIES_GROUP_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$SPECIES_GROUP),])
}

#'Factorizes the species category codes in a data frame
#'@param data The data frame whose \code{SPECIES_CATEGORY_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{SPECIES_CATEGORY_CODE} factorized according to the default sequence of species group codes
#'@export
factorize_species_categories = function(data, connection = DB_IOTDB()) {
  if(!(SPECIES_CATEGORY_CODE %in% colnames(data))) {
    return (data)
  }

  SC = all_species_categories(connection)[order(+SORT)]

  levels = as.character(SC$CODE)
  labels = as.character(SC$NAME_EN)

  data$SPECIES_CATEGORY = data$SPECIES_CATEGORY_CODE

  data$SPECIES_CATEGORY =
    factor(
      data$SPECIES_CATEGORY,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$SPECIES_CATEGORY_CODE =
    factor(
      data$SPECIES_CATEGORY_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$SPECIES_CATEGORY),])
}

#'Factorizes the species codes in a data frame
#'@param data The data frame whose \code{SPECIES_CODE} column should be factorized
#'@param connection A connection to the \code{\link{IOTDB}}
#'@return the original data frame with the \code{SPECIES_CODE} factorized according to the default sequence of species codes
#'@export
factorize_species = function(data, connection = DB_IOTDB()) {
  if(!(SPECIES_CODE %in% colnames(data))) {
    return (data)
  }

  main_species = main_species_data(connection)

  levels = append(unique(main_species[!is.na(SORT)]$CODE), "UNCL")
  labels = append(unique(main_species[!is.na(SORT)]$NAME_EN), "Unclassified")

  data[!SPECIES_CODE %in% levels]$SPECIES_CODE = "UNCL"

  data$SPECIES = data$SPECIES_CODE

  data$SPECIES =
    factor(
      data$SPECIES,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$SPECIES_CODE =
    factor(
      data$SPECIES_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$SPECIES),])
}

#'Factorizes the species codes in a data frame
#'@param data The data frame whose \code{IUCN_STATUS} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{IUCN_STATUS} factorized according to the default sequence of IUCN status codes
#'@export
factorize_IUCN_status = function(data, connection = DB_IOTDB()) {
  if(!(IUCN_STATUS_CODE %in% colnames(data))) {
    return (data)
  }

  data[is.na(IUCN_STATUS_CODE)]$IUCN_STATUS_CODE = "UNCL"

  IU = all_codes("IUCN_STATUS", connection)[order(+SORT)]

  levels = as.character(IU$CODE)
  labels = as.character(IU$NAME_EN)

  data$IUCN_STATUS = data$IUCN_STATUS_CODE

  data$IUCN_STATUS =
    factor(
      data$IUCN_STATUS,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$IUCN_STATUS_CODE =
    factor(
      data$IUCN_STATUS_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$IUCN_STATUS),])
}

#'Factorizes the raising codes in a data frame
#'@param data The data frame whose \code{RAISE_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{RAISE_CODE} factorized according to the default sequence of raising codes
#'@export
factorize_raisings = function(data, connection = DB_IOTDB()) {
  if(!(RAISE_CODE %in% colnames(data))) {
    return (data)
  }

  data[is.na(RAISE_CODE)]$RAISE_CODE = "UNCL"

  RA = all_codes("RAISINGS", connection)[order(+SORT)]

  levels = as.character(RA$CODE)
  labels = as.character(RA$NAME_EN)

  data$RAISING = data$RAISE_CODE

  data$RAISING =
    factor(
      data$RAISING,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$RAISE_CODE =
    factor(
      data$RAISE_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$RAISING),])
}

#'Factorizes the ROS event type codes in a data frame
#'@param data The data frame whose \code{EVENT_TYPE_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{EVENT_TYPE_CODE} factorized according to the default sequence of event type codes
#'@export
factorize_ROS_event_types = function(data) {
  if(!(EVENT_TYPE_CODE %in% colnames(data))) {
    return (data)
  }

  data[is.na(EVENT_TYPE_CODE)]$EVENT_TYPE_CODE = "UNCL"

  levels = c("SETTING", "HAULING")

  data$EVENT_TYPE = data$EVENT_TYPE_CODE
  data$EVENT_TYPE =
    factor(
      data$EVENT_TYPE,
      levels = levels,
      labels = c("Setting", "Hauling"),
      ordered = TRUE
    )

  data$EVENT_TYPE_CODE =
    factor(
      data$EVENT_TYPE_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$EVENT_TYPE),])
}

#'Factorizes the fishery type codes in a data frame
#'@param data The data frame whose \code{FISHERY_TYPE_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{FISHERY_TYPE_CODE} factorized according to the default sequence of fishery type codes
#'@export
factorize_fishery_types = function(data, connection = DB_IOTDB()) {
  if(!(FISHERY_TYPE_CODE %in% colnames(data))) {
    return (data)
  }

  FT = all_codes("FISHERY_TYPES", connection)[order(+SORT)]

  levels = as.character(FT$CODE)
  labels = as.character(FT$NAME_EN)

  data$FISHERY_TYPE = data$FISHERY_TYPE_CODE

  data$FISHERY_TYPE =
    factor(
      data$FISHERY_TYPE,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$FISHERY_TYPE_CODE =
    factor(
      data$FISHERY_TYPE_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$FISHERY_TYPE),])
}

#'Factorizes the fishing ground codes (\code{IRWESIO} and \code{IREASIO}) in a data frame
#'@param data The data frame whose \code{FISHING_GROUND_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{FISHING_GROUND_CODE} factorized according to the default sequence of fishing ground codes
#'@export
factorize_fishing_grounds = function(data, connection = DB_IOTDB()) {
  if(!(FISHING_GROUND_CODE %in% colnames(data))) {
    return (data)
  }

  FG = all_codes("FISHING_GROUNDS", connection)[order(+SORT)]

  levels = as.character(FG$CODE)
  labels = as.character(FG$NAME_EN)

  data$FISHING_GROUND = data$FISHING_GROUND_CODE

  data$FISHING_GROUND =
    factor(
      data$FISHING_GROUND,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$FISHING_GROUND_CODE =
    factor(
      data$FISHING_GROUND_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$FISHING_GROUND),])
}

#'Factorizes the fishery group codes in a data frame
#'@param data The data frame whose \code{FISHERY_GROUP_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{FISHERY_GROUP_CODE} factorized according to the default sequence of fishery group codes
#'@export
factorize_fishery_groups = function(data, connection = DB_IOTDB()) {
  if(!(FISHERY_GROUP_CODE %in% colnames(data))) {
    return (data)
  }

  FG = all_codes("FISHERY_GROUPS", connection)[order(+SORT)]

  levels = as.character(FG$CODE)
  labels = as.character(FG$NAME_EN)

  data$FISHERY_GROUP = data$FISHERY_GROUP_CODE

  data$FISHERY_GROUP =
    factor(
      data$FISHERY_GROUP,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$FISHERY_GROUP_CODE =
    factor(
      data$FISHERY_GROUP_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$FISHERY_GROUP),])
}

#'Factorizes the fishery codes in a data frame
#'@param data The data frame whose \code{FISHERY_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{FISHERY_CODE} factorized according to the default sequence of fishery codes
#'@export
factorize_fisheries = function(data, connection = DB_IOTDB()) {
  if(!(FISHERY_CODE %in% colnames(data))) {
    return (data)
  }

  FI = all_codes("FISHERIES", connection)[order(+SORT)]

  levels = as.character(FI$CODE)
  labels = as.character(FI$NAME_EN)

  data$FISHERY = data$FISHERY_CODE

  data$FISHERY =
    factor(
      data$FISHERY,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$FISHERY_CODE =
    factor(
      data$FISHERY_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$FISHERY),])
}

#'Factorizes the gear codes in a data frame
#'@param data The data frame whose \code{GEAR_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{GEAR_CODE} factorized according to the default sequence of gear codes
#'@export
factorize_gears = function(data, connection = DB_IOTDB()) {
  if(!(GEAR_CODE %in% colnames(data))) {
    return (data)
  }

  FI = all_codes("GEARS", connection)[USED == TRUE][order(+SORT)]

  levels = as.character(FI$CODE)
  labels = as.character(FI$NAME_EN)

  data$GEAR = data$GEAR_CODE

  data$GEAR =
    factor(
      data$GEAR,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$GEAR_CODE =
    factor(
      data$GEAR_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$GEAR),])
}

#'Factorizes the quarters in a data frame
#'@param data The data frame whose \code{QUARTER} column should be factorized
#'@return the original data frame with the \code{QUARTER} factorized according to the default sequence of quarters
#'@export
factorize_quarters = function(data) {
  levels = c("Q1", "Q2", "Q3", "Q4", "UNCL")

  data$QUARTER =
    factor(
      data$QUARTER,
      levels = levels,
      labels = levels,
      ordered = TRUE
    )

  return (data[order(data$QUARTER),])
}

#'Factorizes the fleet codes in a data frame
#'@param data The data frame whose \code{FLEET_CODE} column should be factorized
#'@param connection A connection to the IOTDB (required to retrieve fleet data)
#'@return the original data frame with the \code{FLEET_CODE} factorized according to the default sequence of fleet codes
#'@export
factorize_fleets = function(data, connection = DB_IOTDB()) {
  FL = all_fleets(connection)[order(+SORT)]

  levels = unique(as.character(FL$CODE))
  labels = unique(as.character(FL$NAME_EN))

  data$FLEET_CODE =
    factor(
      data$FLEET_CODE,
      levels = levels,
      ordered = TRUE
    )

  data$FLEET = data$FLEET_CODE
  data$FLEET =
    factor(
      data$FLEET,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  return (data[order(data$FLEET_CODE),])
}

#'Factorizes the quality codes in a data frame.
#'This function is unnecessary, as the regular NC, EF, CA, CE and SA datasets do not come with a \code{QUALITY_CODE} column.
#'The only place where quality codes are leveraged is during the creation of dataset-specific quality charts, which include already
#'the logic to factorize the colors by quality code.
#'@param data The data frame whose \code{QUALITY_CODE} column should be factorized
#'@param connection A connection to the IOTDB (required for the factorization of fleets)
#'@return the original data frame with the \code{QUALITY_CODE} factorized according to the default sequence of quality codes
#'@export
factorize_qualities = function(data) {
  if(!(QUALITY_CODE %in% colnames(data))) {
    return (data)
  }

  levels = sort(unique(data$QUALITY_CODE))

  data$QUALITY_CODE =
    factor(
      data$QUALITY_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data)
}

#'Factorizes the fate codes in a data frame
#'@param data The data frame whose \code{FATE_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{FATE_CODE} factorized according to the default sequence of fate codes
#'@export
factorize_fates = function(data, connection = DB_IOTDB()) {
  if(!(FATE_CODE %in% colnames(data))) {
    return (data)
  }

  data[is.na(FATE_CODE)]$FATE_CODE = "UNCL"

  FA = all_codes("FATES", connection)[order(+SORT)]

  levels = as.character(FA$CODE)
  labels = as.character(FA$NAME_EN)

  data$FATE = data$FATE_CODE

  data$FATE =
    factor(
      data$FATE,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$FATE_CODE =
    factor(
      data$FATE_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$FATE),])
}

#'Factorizes the fate type codes in a data frame
#'@param data The data frame whose \code{FATE_TYPE_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{FATE_TYPE_CODE} factorized according to the default sequence of fate type codes
#'@export
factorize_fate_types = function(data, connection = DB_IOTDB()) {
  if(!(FATE_TYPE_CODE %in% colnames(data))) {
    return (data)
  }

  data[is.na(FATE_TYPE_CODE)]$FATE_TYPE_CODE = "UNCL"

  FT = all_codes("FATE_TYPES", connection)[order(+SORT)]

  levels = as.character(FT$CODE)
  labels = as.character(FT$NAME_EN)

  data$FATE_TYPE = data$FATE_TYPE_CODE

  data$FATE_TYPE =
    factor(
      data$FATE_TYPE,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$FATE_TYPE_CODE =
    factor(
      data$FATE_TYPE_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$FATE_TYPE),])
}

#'Factorizes the condition codes in a data frame
#'@param data The data frame whose \code{CONDITION_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{CONDITION_CODE} factorized according to the default sequence of condition codes
#'@export
factorize_conditions = function(data, connection = DB_IOTDB()) {
  if(!(CONDITION_CODE %in% colnames(data))) {
    return (data)
  }

  data[is.na(CONDITION_CODE)]$CONDITION_CODE = "UNCL"

  CO = all_codes("CONDITIONS", connection)[order(+SORT)]

  levels = as.character(CO$CODE)
  labels = as.character(CO$NAME_EN)

  data$CONDITION = data$CONDITION_CODE

  data$CONDITION =
    factor(
      data$CONDITION,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$CONDITION_CODE =
    factor(
      data$CONDITION_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$CONDITION),])
}

#'Factorizes the condition type codes in a data frame
#'@param data The data frame whose \code{CONDITION_TYPE_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{CONDITION_TYPE_CODE} factorized according to the default sequence of condition type codes
#'@export
factorize_condition_types = function(data, connection = DB_IOTDB()) {
  if(!(CONDITION_TYPE_CODE %in% colnames(data))) {
    return (data)
  }

  data[is.na(CONDITION_TYPE_CODE)]$CONDITION_TYPE_CODE = "UNCL"

  CT = all_codes("CONDITION_TYPES", connection)[order(+SORT)]

  levels = as.character(CT$CODE)
  labels = as.character(CT$NAME_EN)

  data$CONDITION_TYPE = data$CONDITION_TYPE_CODE

  data$CONDITION_TYPE =
    factor(
      data$CONDITION_TYPE,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  data$CONDITION_TYPE_CODE =
    factor(
      data$CONDITION_TYPE_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (data[order(data$CONDITION_TYPE),])
}

factorize_others = function(data) {
  columns = colnames(data)

  #YEAR should not be factorized
  #data$YEAR = factor(data$YEAR)

  #As shouldn't MONTH_START and MONTH_END
  #if("MONTH_START" %in% columns) data$MONTH_START = factor(data$MONTH_START)
  #if("MONTH_END" %in% columns) data$MONTH_END = factor(data$MONTH_END)

  #data$GEAR_CODE = factor(data$GEAR_CODE, ordered = TRUE)
  #data$FISHING_GROUND_CODE = factor(data$FISHING_GROUND_CODE, ordered = TRUE)

  if(FLAG_CODE %in% columns) data$FLAG_CODE = factor(data$FLAG_CODE, ordered = TRUE)

  if(SCHOOL_TYPE_CODE %in% columns) data$SCHOOL_TYPE_CODE = factor(data$SCHOOL_TYPE_CODE, ordered = TRUE)
  if(EFFORT_SCHOOL_TYPE_CODE %in% columns) data$EFFORT_SCHOOL_TYPE_CODE = factor(data$EFFORT_SCHOOL_TYPE_CODE, ordered = TRUE)
  if(CATCH_SCHOOL_TYPE_CODE %in% columns) data$CATCH_SCHOOL_TYPE_CODE = factor(data$CATCH_SCHOOL_TYPE_CODE, ordered = TRUE)

  if(EFFORT_UNIT_CODE %in% columns) data$EFFORT_UNIT_CODE = factor(data$EFFORT_UNIT_CODE, ordered = TRUE)

  if(MEASURE_TYPE_CODE %in% columns) data$MEASURE_TYPE_CODE = factor(data$MEASURE_TYPE_CODE, ordered = TRUE)
  if(SEX_CODE %in% columns) data$SEX_CODE = factor(data$SEX_CODE, ordered = TRUE)

  #if(RAISE_CODE %in% columns) data$RAISE_CODE = factor(data$RAISE_CODE, ordered = TRUE)

  if(MEASURE_TYPE_CODE %in% columns) data$MEASURE_TYPE_CODE = factor(data$MEASURE_TYPE_CODE, ordered = TRUE)
  if(MEASURE_UNIT_CODE %in% columns) data$MEASURE_UNIT_CODE = factor(data$MEASURE_UNIT_CODE, ordered = TRUE)

  if(LENGTH_MEASURE_TYPE_CODE %in% columns) data$LENGTH_MEASURE_TYPE_CODE = factor(data$LENGTH_MEASURE_TYPE_CODE, ordered = TRUE)
  if(LENGTH_MEASURE_UNIT_CODE %in% columns) data$LENGTH_MEASURE_UNIT_CODE = factor(data$LENGTH_MEASURE_UNIT_CODE, ordered = TRUE)

  if(WEIGHT_MEASURE_TYPE_CODE %in% columns) data$WEIGHT_MEASURE_TYPE_CODE = factor(data$WEIGHT_MEASURE_TYPE_CODE, ordered = TRUE)
  if(WEIGHT_MEASURE_UNIT_CODE %in% columns) data$WEIGHT_MEASURE_UNIT_CODE = factor(data$WEIGHT_MEASURE_UNIT_CODE, ordered = TRUE)

  return (data)
}

factorize_all = function(data, connection = DB_IOTDB()) {
  return (
    factorize_others(
      factorize_ROS_event_types(
        factorize_raisings(
          factorize_conditions(
            factorize_condition_types(
              factorize_fates(
                factorize_fate_types(
                  factorize_species(
                    factorize_species_categories(
                      factorize_species_groups(
                        factorize_species_wps(
                          factorize_IUCN_status(
                            factorize_gears(
                              factorize_fisheries(
                                factorize_fishery_groups(
                                  factorize_fishery_types(
                                    factorize_fleets(
                                      #factorize_fishing_grounds(
                                        factorize_quarters(
                                          data
                                        ),
                                        connection
                                      #), connection
                                    ), connection
                                  ), connection
                                ), connection
                              ), connection
                            ), connection
                          ), connection
                        ), connection
                      ), connection
                    ), connection
                  ), connection
                ), connection
              ), connection
            ), connection
          ), connection
        )
      )
    )
  )
}
