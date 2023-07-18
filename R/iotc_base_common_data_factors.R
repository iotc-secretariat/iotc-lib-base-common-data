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
#'@param to_factorize The data frame whose \code{SPECIES_WP_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{SPECIES_WP_CODE} factorized according to the default sequence of species group codes
#'@export
factorize_species_wps = function(to_factorize, connection = DB_IOTDB()) {
  if(!(C_SPECIES_WP_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  WPS = all_wps(connection)[order(+SORT)]

  levels = as.character(WPS$CODE)
  labels = as.character(WPS$NAME_EN)

  to_factorize$SPECIES_WP = to_factorize$SPECIES_WP_CODE

  to_factorize$SPECIES_WP =
    factor(
      to_factorize$SPECIES_WP,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$SPECIES_WP_CODE =
    factor(
      to_factorize$SPECIES_WP_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$SPECIES_WP),])
}

#'Factorizes the species group codes in a data frame
#'@param to_factorize The data frame whose \code{SPECIES_GROUP_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{SPECIES_GROUP_CODE} factorized according to the default sequence of species group codes
#'@export
factorize_species_groups = function(to_factorize, connection = DB_IOTDB()) {
  if(!(C_SPECIES_GROUP_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  SG = all_species_groups(connection)[order(+SORT)]

  levels = as.character(SG$CODE)
  labels = as.character(SG$NAME_EN)

  to_factorize$SPECIES_GROUP = to_factorize$SPECIES_GROUP_CODE

  to_factorize$SPECIES_GROUP =
    factor(
      to_factorize$SPECIES_GROUP,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$SPECIES_GROUP_CODE =
    factor(
      to_factorize$SPECIES_GROUP_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$SPECIES_GROUP),])
}

#'Factorizes the species category codes in a data frame
#'@param to_factorize The data frame whose \code{SPECIES_CATEGORY_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{SPECIES_CATEGORY_CODE} factorized according to the default sequence of species group codes
#'@export
factorize_species_categories = function(to_factorize, connection = DB_IOTDB()) {
  if(!(C_SPECIES_CATEGORY_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  SC = all_species_categories(connection)[order(+SORT)]

  levels = as.character(SC$CODE)
  labels = as.character(SC$NAME_EN)

  to_factorize$SPECIES_CATEGORY = to_factorize$SPECIES_CATEGORY_CODE

  to_factorize$SPECIES_CATEGORY =
    factor(
      to_factorize$SPECIES_CATEGORY,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$SPECIES_CATEGORY_CODE =
    factor(
      to_factorize$SPECIES_CATEGORY_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$SPECIES_CATEGORY),])
}

#'Factorizes the species codes in a data frame
#'@param to_factorize The data frame whose \code{SPECIES_CODE} column should be factorized
#'@param collapse_secondary_species If \code{TRUE} all species other than the main ones will be assigned to \code{UNCL} / \code{Unclassified}
#'@param connection A connection to the \code{\link{IOTDB}}
#'@return the original data frame with the \code{SPECIES_CODE} factorized according to the default sequence of species codes
#'@export
factorize_species = function(to_factorize, collapse_secondary_species = FALSE, connection = DB_IOTDB()) {
  if(!(C_SPECIES_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  if(C_SPECIES %in% colnames(to_factorize)) {
    to_factorize[is.na(SPECIES), SPECIES := "<unknown species name>"]
  }

  main_species  = main_species_data(connection)[!is.na(SORT)]
  levels = unique(main_species[!is.na(SORT)]$CODE)
  labels = unique(main_species[!is.na(SORT)]$NAME_EN)

  if(collapse_secondary_species) {
    levels = append(levels, "UNCL")
    labels = append(labels, "Unclassified")

    to_factorize[!SPECIES_CODE %in% levels,
                 `:=`(SPECIES_CODE          = "UNCL",
                      SPECIES               = "Unclassified",
                      SPECIES_SCIENTIFIC    = NA,
                      SPECIES_FAMILY        = NA,
                      SPECIES_ORDER         = NA,
                      SPECIES_WP_CODE       = "UNCL",              # To be kept updated with the actual WP codes
                      SPECIES_WP            = "Unclassified",      # To be kept updated with the actual WP names
                      SPECIES_GROUP_CODE    = "OTHERS",            # To be kept updated with the actual species group codes
                      SPECIES_GROUP         = "All other species", # To be kept updated with the actual species group names
                      SPECIES_CATEGORY_CODE = "OTHERS",            # To be kept updated with the actual species category codes
                      SPECIES_CATEGORY      = "Other species NEI", # To be kept updated with the actual species category names
                      IS_IOTC_SPECIES       = NA,
                      IS_SPECIES_AGGREGATE  = NA,
                      IS_SSI                = NA,
                      IUCN_STATUS_CODE      = NA,
                      IUCN_STATUS           = NA)]
  } else {
    other_species = unique(to_factorize[!SPECIES_CODE %in% main_species$CODE, .(SPECIES_CODE, SPECIES)])[order(SPECIES)]
    levels = append(levels, other_species$SPECIES_CODE)
    labels = append(labels, other_species$SPECIES)
  }

  to_factorize$SPECIES = to_factorize$SPECIES_CODE

  to_factorize$SPECIES =
    factor(
      to_factorize$SPECIES,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$SPECIES_CODE =
    factor(
      to_factorize$SPECIES_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$SPECIES),])
}

#'Factorizes the IUCN status codes in a data frame
#'@param to_factorize The data frame whose \code{IUCN_STATUS} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{IUCN_STATUS} factorized according to the default sequence of IUCN status codes
#'@export
factorize_IUCN_status = function(to_factorize, connection = DB_IOTDB()) {
  if(!(C_IUCN_STATUS_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  to_factorize[is.na(IUCN_STATUS_CODE)]$IUCN_STATUS_CODE = "UNCL"

  IU = all_codes("IUCN_STATUS", connection)[order(+SORT)]

  levels = as.character(IU$CODE)
  labels = as.character(IU$NAME_EN)

  to_factorize$IUCN_STATUS = to_factorize$IUCN_STATUS_CODE

  to_factorize$IUCN_STATUS =
    factor(
      to_factorize$IUCN_STATUS,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$IUCN_STATUS_CODE =
    factor(
      to_factorize$IUCN_STATUS_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$IUCN_STATUS),])
}

#'Factorizes the raising codes in a data frame
#'@param to_factorize The data frame whose \code{RAISE_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{RAISE_CODE} factorized according to the default sequence of raising codes
#'@export
factorize_raisings = function(to_factorize, connection = DB_IOTDB()) {
  if(!(C_RAISE_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  to_factorize[is.na(RAISE_CODE)]$RAISE_CODE = "UNCL"

  RA = all_codes("RAISINGS", connection)[order(+SORT)]

  levels = as.character(RA$CODE)
  labels = as.character(RA$NAME_EN)

  to_factorize$RAISING = to_factorize$RAISE_CODE

  to_factorize$RAISING =
    factor(
      to_factorize$RAISING,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$RAISE_CODE =
    factor(
      to_factorize$RAISE_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$RAISING),])
}

#'Factorizes the ROS event type codes in a data frame
#'@param to_factorize The data frame whose \code{EVENT_TYPE_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{EVENT_TYPE_CODE} factorized according to the default sequence of event type codes
#'@export
factorize_ROS_event_types = function(to_factorize) {
  if(!(C_EVENT_TYPE_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  to_factorize[is.na(EVENT_TYPE_CODE)]$EVENT_TYPE_CODE = "UNCL"

  levels = c("SETTING", "HAULING")

  to_factorize$EVENT_TYPE = to_factorize$EVENT_TYPE_CODE
  to_factorize$EVENT_TYPE =
    factor(
      to_factorize$EVENT_TYPE,
      levels = levels,
      labels = c("Setting", "Hauling"),
      ordered = TRUE
    )

  to_factorize$EVENT_TYPE_CODE =
    factor(
      to_factorize$EVENT_TYPE_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$EVENT_TYPE),])
}

#'Factorizes the fishery type codes in a data frame
#'@param to_factorize The data frame whose \code{FISHERY_TYPE_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{FISHERY_TYPE_CODE} factorized according to the default sequence of fishery type codes
#'@export
factorize_fishery_types = function(to_factorize, connection = DB_IOTDB()) {
  if(!(C_FISHERY_TYPE_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  FT = all_codes("FISHERY_TYPES", connection)[order(+SORT)]

  levels = as.character(FT$CODE)
  labels = as.character(FT$NAME_EN)

  to_factorize$FISHERY_TYPE = to_factorize$FISHERY_TYPE_CODE

  to_factorize$FISHERY_TYPE =
    factor(
      to_factorize$FISHERY_TYPE,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$FISHERY_TYPE_CODE =
    factor(
      to_factorize$FISHERY_TYPE_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$FISHERY_TYPE),])
}

#'Factorizes the fishing ground codes (\code{IRWESIO} and \code{IREASIO}) in a data frame
#'@param to_factorize The data frame whose \code{FISHING_GROUND_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{FISHING_GROUND_CODE} factorized according to the default sequence of fishing ground codes
#'@export
factorize_fishing_grounds = function(to_factorize, connection = DB_IOTDB()) {
  if(!(C_FISHING_GROUND_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  FG = all_codes("FISHING_GROUNDS", connection)[order(+SORT)]

  levels = as.character(FG$CODE)
  labels = as.character(FG$NAME_EN)

  to_factorize$FISHING_GROUND = to_factorize$FISHING_GROUND_CODE

  to_factorize$FISHING_GROUND =
    factor(
      to_factorize$FISHING_GROUND,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$FISHING_GROUND_CODE =
    factor(
      to_factorize$FISHING_GROUND_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$FISHING_GROUND),])
}

#'Factorizes the fishery group codes in a data frame
#'@param to_factorize The data frame whose \code{FISHERY_GROUP_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{FISHERY_GROUP_CODE} factorized according to the default sequence of fishery group codes
#'@export
factorize_fishery_groups = function(to_factorize, connection = DB_IOTDB()) {
  if(!(C_FISHERY_GROUP_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  FG = all_codes("FISHERY_GROUPS", connection)[order(+SORT)]

  levels = as.character(FG$CODE)
  labels = as.character(FG$NAME_EN)

  to_factorize$FISHERY_GROUP = to_factorize$FISHERY_GROUP_CODE

  to_factorize$FISHERY_GROUP =
    factor(
      to_factorize$FISHERY_GROUP,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$FISHERY_GROUP_CODE =
    factor(
      to_factorize$FISHERY_GROUP_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$FISHERY_GROUP),])
}

#'Factorizes the fishery codes in a data frame
#'@param to_factorize The data frame whose \code{FISHERY_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{FISHERY_CODE} factorized according to the default sequence of fishery codes
#'@export
factorize_fisheries = function(to_factorize, connection = DB_IOTDB()) {
  if(!(C_FISHERY_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  FI = all_codes("FISHERIES", connection)[order(+SORT)]

  levels = as.character(FI$CODE)
  labels = as.character(FI$NAME_EN)

  to_factorize$FISHERY = to_factorize$FISHERY_CODE

  to_factorize$FISHERY =
    factor(
      to_factorize$FISHERY,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$FISHERY_CODE =
    factor(
      to_factorize$FISHERY_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$FISHERY),])
}

#'Factorizes the gear codes in a data frame
#'@param to_factorize The data frame whose \code{GEAR_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{GEAR_CODE} factorized according to the default sequence of gear codes
#'@export
factorize_gears = function(to_factorize, connection = DB_IOTDB()) {
  if(!(C_GEAR_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  FI = all_codes("GEARS", connection)[USED == TRUE][order(+SORT)]

  levels = as.character(FI$CODE)
  labels = as.character(FI$NAME_EN)

  to_factorize$GEAR = to_factorize$GEAR_CODE

  to_factorize$GEAR =
    factor(
      to_factorize$GEAR,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$GEAR_CODE =
    factor(
      to_factorize$GEAR_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$GEAR),])
}

#'Factorizes the quarters in a data frame
#'@param to_factorize The data frame whose \code{QUARTER} column should be factorized
#'@return the original data frame with the \code{QUARTER} factorized according to the default sequence of quarters
#'@export
factorize_quarters = function(to_factorize) {
  levels = c("Q1", "Q2", "Q3", "Q4", "UNCL")

  to_factorize$QUARTER =
    factor(
      to_factorize$QUARTER,
      levels = levels,
      labels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$QUARTER),])
}

#'Factorizes the fleet codes in a data frame
#'@param to_factorize The data frame whose \code{FLEET_CODE} column should be factorized
#'@param connection A connection to the IOTDB (required to retrieve fleet data)
#'@return the original data frame with the \code{FLEET_CODE} factorized according to the default sequence of fleet codes
#'@export
factorize_fleets = function(to_factorize, connection = DB_IOTDB()) {
  FL = all_fleets(connection)[order(+SORT)]

  levels = unique(as.character(FL$CODE))
  labels = unique(as.character(FL$NAME_EN))

  to_factorize$FLEET_CODE =
    factor(
      to_factorize$FLEET_CODE,
      levels = levels,
      ordered = TRUE
    )

  to_factorize$FLEET = to_factorize$FLEET_CODE
  to_factorize$FLEET =
    factor(
      to_factorize$FLEET,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$FLEET_CODE),])
}

#'Factorizes the quality codes in a data frame.
#'This function is unnecessary, as the regular NC, EF, CA, CE and SA datasets do not come with a \code{QUALITY_CODE} column.
#'The only place where quality codes are leveraged is during the creation of dataset-specific quality charts, which include already
#'the logic to factorize the colors by quality code.
#'@param to_factorize The data frame whose \code{QUALITY_CODE} column should be factorized
#'@param connection A connection to the IOTDB (required for the factorization of fleets)
#'@return the original data frame with the \code{QUALITY_CODE} factorized according to the default sequence of quality codes
#'@export
factorize_qualities = function(to_factorize) {
  if(!(C_QUALITY_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  levels = sort(unique(to_factorize$QUALITY_CODE))

  to_factorize$QUALITY_CODE =
    factor(
      to_factorize$QUALITY_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize)
}

#'Factorizes the fate codes in a data frame
#'@param to_factorize The data frame whose \code{FATE_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{FATE_CODE} factorized according to the default sequence of fate codes
#'@export
factorize_fates = function(to_factorize, connection = DB_IOTDB()) {
  if(!(C_FATE_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  to_factorize[is.na(FATE_CODE)]$FATE_CODE = "UNCL"

  FA = all_codes("FATES", connection)[order(+SORT)]

  levels = as.character(FA$CODE)
  labels = as.character(FA$NAME_EN)

  to_factorize$FATE = to_factorize$FATE_CODE

  to_factorize$FATE =
    factor(
      to_factorize$FATE,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$FATE_CODE =
    factor(
      to_factorize$FATE_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$FATE),])
}

#'Factorizes the fate type codes in a data frame
#'@param to_factorize The data frame whose \code{FATE_TYPE_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{FATE_TYPE_CODE} factorized according to the default sequence of fate type codes
#'@export
factorize_fate_types = function(to_factorize, connection = DB_IOTDB()) {
  if(!(C_FATE_TYPE_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  to_factorize[is.na(FATE_TYPE_CODE)]$FATE_TYPE_CODE = "UNCL"

  FT = all_codes("FATE_TYPES", connection)[order(+SORT)]

  levels = as.character(FT$CODE)
  labels = as.character(FT$NAME_EN)

  to_factorize$FATE_TYPE = to_factorize$FATE_TYPE_CODE

  to_factorize$FATE_TYPE =
    factor(
      to_factorize$FATE_TYPE,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$FATE_TYPE_CODE =
    factor(
      to_factorize$FATE_TYPE_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$FATE_TYPE),])
}

#'Factorizes the condition codes in a data frame
#'@param to_factorize The data frame whose \code{CONDITION_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{CONDITION_CODE} factorized according to the default sequence of condition codes
#'@export
factorize_conditions = function(to_factorize, connection = DB_IOTDB()) {
  if(!(C_CONDITION_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  to_factorize[is.na(CONDITION_CODE)]$CONDITION_CODE = "UNCL"

  CO = all_codes("CONDITIONS", connection)[order(+SORT)]

  levels = as.character(CO$CODE)
  labels = as.character(CO$NAME_EN)

  to_factorize$CONDITION = to_factorize$CONDITION_CODE

  to_factorize$CONDITION =
    factor(
      to_factorize$CONDITION,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$CONDITION_CODE =
    factor(
      to_factorize$CONDITION_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$CONDITION),])
}

#'Factorizes the condition type codes in a data frame
#'@param to_factorize The data frame whose \code{CONDITION_TYPE_CODE} column should be factorized
#'@param connection A connection to \code{\link{IOTDB}}
#'@return the original data frame with the \code{CONDITION_TYPE_CODE} factorized according to the default sequence of condition type codes
#'@export
factorize_condition_types = function(to_factorize, connection = DB_IOTDB()) {
  if(!(C_CONDITION_TYPE_CODE %in% colnames(to_factorize))) {
    return (to_factorize)
  }

  to_factorize[is.na(CONDITION_TYPE_CODE)]$CONDITION_TYPE_CODE = "UNCL"

  CT = all_codes("CONDITION_TYPES", connection)[order(+SORT)]

  levels = as.character(CT$CODE)
  labels = as.character(CT$NAME_EN)

  to_factorize$CONDITION_TYPE = to_factorize$CONDITION_TYPE_CODE

  to_factorize$CONDITION_TYPE =
    factor(
      to_factorize$CONDITION_TYPE,
      levels = levels,
      labels = labels,
      ordered = TRUE
    )

  to_factorize$CONDITION_TYPE_CODE =
    factor(
      to_factorize$CONDITION_TYPE_CODE,
      levels = levels,
      ordered = TRUE
    )

  return (to_factorize[order(to_factorize$CONDITION_TYPE),])
}

factorize_others = function(to_factorize) {
  columns = colnames(to_factorize)

  #YEAR should not be factorized
  #to_factorize$YEAR = factor(to_factorize$YEAR)

  #As shouldn't MONTH_START and MONTH_END
  #if("MONTH_START" %in% columns) to_factorize$MONTH_START = factor(to_factorize$MONTH_START)
  #if("MONTH_END" %in% columns) to_factorize$MONTH_END = factor(to_factorize$MONTH_END)

  #to_factorize$GEAR_CODE = factor(to_factorize$GEAR_CODE, ordered = TRUE)
  #to_factorize$FISHING_GROUND_CODE = factor(to_factorize$FISHING_GROUND_CODE, ordered = TRUE)

  if(C_FLAG_CODE %in% columns) to_factorize$FLAG_CODE = factor(to_factorize$FLAG_CODE, ordered = TRUE)

  if(C_SCHOOL_TYPE_CODE %in% columns) to_factorize$SCHOOL_TYPE_CODE = factor(to_factorize$SCHOOL_TYPE_CODE, ordered = TRUE)
  if(C_EFFORT_SCHOOL_TYPE_CODE %in% columns) to_factorize$EFFORT_SCHOOL_TYPE_CODE = factor(to_factorize$EFFORT_SCHOOL_TYPE_CODE, ordered = TRUE)
  if(C_CATCH_SCHOOL_TYPE_CODE %in% columns) {
    to_factorize$CATCH_SCHOOL_TYPE_CODE = as.character(to_factorize$CATCH_SCHOOL_TYPE_CODE)
    #Assigns 'UNCL' to catch school type codes which are not set, otherwise it will trigger a bug with data.table
    to_factorize[is.na(CATCH_SCHOOL_TYPE_CODE)]$CATCH_SCHOOL_TYPE_CODE = "UNCL"

    to_factorize$CATCH_SCHOOL_TYPE_CODE = factor(to_factorize$CATCH_SCHOOL_TYPE_CODE, ordered = TRUE)
  }

  if(C_EFFORT_UNIT_CODE %in% columns) to_factorize$EFFORT_UNIT_CODE = factor(to_factorize$EFFORT_UNIT_CODE, ordered = TRUE)

  if(C_MEASURE_TYPE_CODE %in% columns) to_factorize$MEASURE_TYPE_CODE = factor(to_factorize$MEASURE_TYPE_CODE, ordered = TRUE)
  if(C_SEX_CODE %in% columns) to_factorize$SEX_CODE = factor(to_factorize$SEX_CODE, ordered = TRUE)

  #if(RAISE_CODE %in% columns) to_factorize$RAISE_CODE = factor(to_factorize$RAISE_CODE, ordered = TRUE)

  if(C_MEASURE_TYPE_CODE %in% columns) to_factorize$MEASURE_TYPE_CODE = factor(to_factorize$MEASURE_TYPE_CODE, ordered = TRUE)
  if(C_MEASURE_UNIT_CODE %in% columns) to_factorize$MEASURE_UNIT_CODE = factor(to_factorize$MEASURE_UNIT_CODE, ordered = TRUE)

  if(C_LENGTH_MEASURE_TYPE_CODE %in% columns) to_factorize$LENGTH_MEASURE_TYPE_CODE = factor(to_factorize$LENGTH_MEASURE_TYPE_CODE, ordered = TRUE)
  if(C_LENGTH_MEASURE_UNIT_CODE %in% columns) to_factorize$LENGTH_MEASURE_UNIT_CODE = factor(to_factorize$LENGTH_MEASURE_UNIT_CODE, ordered = TRUE)

  if(C_WEIGHT_MEASURE_TYPE_CODE %in% columns) to_factorize$WEIGHT_MEASURE_TYPE_CODE = factor(to_factorize$WEIGHT_MEASURE_TYPE_CODE, ordered = TRUE)
  if(C_WEIGHT_MEASURE_UNIT_CODE %in% columns) to_factorize$WEIGHT_MEASURE_UNIT_CODE = factor(to_factorize$WEIGHT_MEASURE_UNIT_CODE, ordered = TRUE)

  return (to_factorize)
}

factorize_all = function(to_factorize, connection = DB_IOTDB()) {
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
                                          to_factorize
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
                    ), connection = connection # This because 'factorize_species' requires an additional argument of 'logical' type
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
