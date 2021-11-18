CODELISTS_CACHE = new.env(hash = TRUE)

#'Resets the content of the codelists cache, triggering a reload from the database
#'the first time a codelist is accessed
#'@export
reset_codelists_cache = function() {
  # Never seen anything so convoluted in my entire life...
  # Once again, R S-U-C-K-S!!!
  rm(list=ls(envir = CODELISTS_CACHE), envir = CODELISTS_CACHE)
}

#'Extracts all details of a metadata table as available on \code{IOTDB}
#'@param connection A connection to \code{\link{IOTDB}}
#'@param table The table
#'@return the content of the table as a data.table
#'@export
all_meta = function(table = "SPECIES", connection = DB_IOTDB()) {
  return(
    query(
        connection,
        query = paste0("SELECT * FROM [meta].", table)
    )
  )
}

#'Extracts (and caches) all details of a metadata codelist table as available on \code{IOTDB}
#'@param connection A connection to \code{\link{IOTDB}}
#'@param codelist The name of a codelist table
#'@return the content of the codelist table as a data.table
#'@export
all_codes = function(codelist = "FLEETS", connection = DB_IOTDB()) {
  return(
    cache_get_or_set(
      CODELISTS_CACHE,
      codelist,
      { all_meta(codelist, connection) },
      hash_key = FALSE
    )
  )
}

reorder_columns = function(data, remove_non_standard_columns = TRUE) {
  columns = colnames(data)

  STANDARD_COLS = c(YEAR,
                    QUARTER,
                    MONTH_START,
                    MONTH_END,
                    FISHING_GROUND_CODE,
                    FISHING_GROUND,
                    FLAG_CODE,
                    FLEET_CODE,
                    FLEET,
                    FISHERY_TYPE_CODE,
                    FISHERY_TYPE,
                    FISHERY_GROUP_CODE,
                    FISHERY_GROUP,
                    FISHERY_CODE,
                    FISHERY,
                    GEAR_CODE,
                    GEAR,
                    SCHOOL_TYPE_CODE,
                    EFFORT_SCHOOL_TYPE_CODE,
                    CATCH_SCHOOL_TYPE_CODE,
                    EFFORT,
                    EFFORT_UNIT_CODE,
                    IUCN_STATUS_CODE,
                    IUCN_STATUS,
                    SPECIES_WP_CODE,
                    SPECIES_WP,
                    SPECIES_GROUP_CODE,
                    SPECIES_GROUP,
                    SPECIES_CATEGORY_CODE,
                    SPECIES_CATEGORY,
                    SPECIES_CODE,
                    SPECIES,
                    "SPECIES_SCIENTIFIC",
                    "SPECIES_FAMILY",
                    "SPECIES_ORDER",
                    IS_IOTC_SPECIES,
                    IS_SPECIES_AGGREGATE,
                    IS_SSI,
                    CATCH,
                    CATCH_UNIT_CODE,
                    CATCH_IN_NUMBERS,
                    FATE_TYPE_CODE,
                    FATE_TYPE,
                    FATE_CODE,
                    FATE,
                    CONDITION_TYPE_CODE,
                    CONDITION_TYPE,
                    CONDITION_CODE,
                    CONDITION,
                    NUM_INTERACTIONS,
                    MEASURE_TYPE_CODE,
                    MEASURE_TYPE,
                    MEASURE_UNIT_CODE,
                    LENGTH_MEASURE_TYPE_CODE,
                    "LENGTH",
                    LENGTH_MEASURE_UNIT_CODE,
                    WEIGHT_MEASURE_TYPE_CODE,
                    "WEIGHT",
                    WEIGHT_MEASURE_UNIT_CODE,
                    SEX_CODE,
                    SAMPLE_SIZE,
  #                 "FIRST_CLASS_LOW",
  #                 "SIZE_INTERVAL",
                    CLASS_LOW,
                    CLASS_HIGH,
                    FISH_COUNT,
  #                  "TOT_NUM_FISH",
  #                  "TOT_KG_FISH",
                    RAISING,
                    RAISE_CODE,

                    "CATCH_CE", # Specific for data quality results
                    "SAMPLES",  # Specific for data quality results
                    "NC",       # Specific for data quality results
                    "CE",       # Specific for data quality results
                    "SF",       # Specific for data quality results

                    "TRIP_UID",        # Specific for ROS raw sets
                    "TRIP_ID",         # Specific for ROS raw sets
                    "SET_ID",          # Specific for ROS raw sets
                    "SET_UID",         # Specific for ROS raw sets
                    "EVENT_TYPE_CODE", # Specific for ROS raw sets
                    "START_TIME",      # Specific for ROS raw sets
                    "START_LON",       # Specific for ROS raw sets
                    "START_LAT",       # Specific for ROS raw sets
                    "END_TIME",        # Specific for ROS raw sets
                    "END_LON",         # Specific for ROS raw sets
                    "END_LAT"          # Specific for ROS raw sets
  )

  ordered = c()

  for(column in STANDARD_COLS)
    if(column %in% columns)
      ordered = c(ordered, column)

  if(!remove_non_standard_columns) {
    for(column in columns)
      if(!column %in% STANDARD_COLS)
        ordered = c(ordered, column)
  }

  #The '..' is specific to data.table
  return (data[, ..ordered])
}


#'Due to a bug in my current version of data.table, you need to ensure the following
#'patch is applied:
#'
#'remotes::install_github("Rdatatable/data.table#4803")
#'
#'otherwise, after the factorization, accessing the data table in some circumstances
#'yields the following error:
#'
#'Error in rbindlist(list(head(x, topn), tail(x, topn)), use.names = FALSE) : STRING_PTR() can only be applied to a 'character', not a 'NULL'
#'@param data A dataset to decorate
#'@param factorize If \code{TRUE}, factorization will be applied
#'@param remove_non_standard_columns If \code{TRUE}, non-standard columns (i.e., not belonging to the data model) will be removed.
#'@param connection A connection to \code{\link{IOTDB}}
#'@export
decorate = function(data, factorize = TRUE, remove_non_standard_columns = TRUE, connection = DB_IOTDB()) {
  result =
    add_missing_data_fields(
      add_species_metadata(
        data,
        connection
      ),
      connection
    )

  if(factorize) result = factorize_all(result, connection)

  return (reorder_columns(result, remove_non_standard_columns))
}

has_column = function(data, column_name) {
  return(column_name %in% colnames(data))
}

add_quarters = function(data) {
  if(has_column(data, MONTH_START)) {
    data$QUARTER = "UNCL"
    data[MONTH_START <= 3, QUARTER := "Q1"]
    data[MONTH_START >= 4  & MONTH_START <= 6, QUARTER := "Q2"]
    data[MONTH_START >= 7  & MONTH_START <= 9, QUARTER := "Q3"]
    data[MONTH_START >= 10 & MONTH_START <=12, QUARTER := "Q4"]
  }

  return(data)
}

add_fishing_grounds = function(data, connection = DB_IOTDB()) {
  if(has_column(data, FISHING_GROUND_CODE) & !has_column(data, FISHING_GROUND)) {
    CL = all_codes("FISHING_GROUNDS", connection)[order(+SORT)][, .(CODE, FISHING_GROUND = NAME_EN)]

    data = merge(data, CL, by.x="FISHING_GROUND_CODE", by.y="CODE", all.x = TRUE)
  }

  return(data)
}

add_fleets = function(data, connection = DB_IOTDB()) {
  if(has_column(data, FLEET_CODE) & !has_column(data, FLEET)) {
    CL = all_codes("FLEETS", connection)[order(+SORT)][, .(CODE, FLEET = NAME_EN)]

    data = merge(data, CL, by.x="FLEET_CODE", by.y="CODE", all.x = TRUE)
  }

  return(data)
}

add_fishery_types = function(data, connection = DB_IOTDB()) {
  if(has_column(data, FISHERY_TYPE_CODE) & !has_column(data, FISHERY_TYPE)) {
    CL = all_codes("FISHERY_TYPES", connection)[order(+SORT)][, .(CODE, FISHERY_TYPE = NAME_EN)]

    data = merge(data, CL, by.x="FISHERY_TYPE_CODE", by.y="CODE", all.x = TRUE)
  }

  return(data)
}

add_fishery_groups = function(data, connection = DB_IOTDB()) {
  if(has_column(data, FISHERY_GROUP_CODE) & !has_column(data, FISHERY_GROUP)) {
    CL = all_codes("FISHERY_GROUPS", connection)[order(+SORT)][, .(CODE, FISHERY_GROUP = NAME_EN)]

    data = merge(data, CL, by.x="FISHERY_GROUP_CODE", by.y="CODE", all.x = TRUE)
  }

  return(data)
}

add_fisheries = function(data, connection = DB_IOTDB()) {
  #Requires the fishery group code to include HL and TL (but no LI) for the sake of assigning the proper fishery codes
  data = add_fishery_codes(data)

  if(has_column(data, FISHERY_CODE) & !has_column(data, FISHERY)) {
    CL = all_codes("FISHERIES", connection)[order(+SORT)][, .(CODE, FISHERY = NAME_EN)]

    data = merge(data, CL, by.x="FISHERY_CODE", by.y="CODE", all.x = TRUE)
  }

  #Updates the fishery group code by assignling the LI fishery group code to all records with a fishery code in (LIC, LIH, LIT)
  #and the OT fishery group code to all records with a fishery code of OT

  data[FISHERY_CODE %in% c("LIC", "LIH", "LIT"), `:=`(FISHERY_GROUP_CODE = "LI", FISHERY_GROUP = "Line")]
  data[FISHERY_CODE == "OT", `:=`(FISHERY_GROUP_CODE = "OT", FISHERY_GROUP = "Other")]

  return(data)
}

add_gears = function(data, connection = DB_IOTDB()) {
  if(has_column(data, GEAR_CODE) & !has_column(data, GEAR)) {
    CL = all_codes("GEARS", connection)[USED == TRUE][order(+SORT)][, .(CODE, GEAR = NAME_EN)]

    data = merge(data, CL, by.x="GEAR_CODE", by.y="CODE", all.x = TRUE)
  }

  #Updates the fishery group code by assignling the LI fishery group code to all records with a fishery code in (LIC, LIH, LIT)
  #and the OT fishery group code to all records with a fishery code of OT

  data[FISHERY_CODE %in% c("LIC", "LIH", "LIT"), `:=`(FISHERY_GROUP_CODE = "LI", FISHERY_GROUP = "Line")]
  data[FISHERY_CODE == "OT", `:=`(FISHERY_GROUP_CODE = "OT", FISHERY_GROUP = "Other")]

  return(data)
}

add_IUCN_status = function(data, connection = DB_IOTDB()) {
  if(has_column(data, IUCN_STATUS_CODE) & !has_column(data, IUCN_STATUS)) {
    CL = all_codes("IUCN_STATUS", connection)
    CL = CL[order(+SORT)][, .(CODE, IUCN_STATUS = NAME_EN)]

    data = merge(data, CL, by.x="IUCN_STATUS_CODE", by.y="CODE", all.x = TRUE)
  }

  return(data)
}

add_species_wps = function(data, connection = DB_IOTDB()) {
  if(has_column(data, SPECIES_WP_CODE) & !has_column(data, SPECIES_WP)) {
    CL = all_codes("WORKING_PARTIES", connection)[order(+SORT)][, .(CODE, SPECIES_WP = NAME_EN)]

    data = merge(data, CL, by.x="SPECIES_WP_CODE", by.y="CODE", all.x = TRUE)
  }

  return(data)
}

add_species_groups = function(data, connection = DB_IOTDB()) {
  if(has_column(data, SPECIES_GROUP_CODE) & !has_column(data, SPECIES_GROUP)) {
    CL = all_codes("SPECIES_GROUPS", connection)[order(+SORT)][, .(CODE, SPECIES_GROUP = NAME_EN)]

    data = merge(data, CL, by.x="SPECIES_GROUP_CODE", by.y="CODE", all.x = TRUE)
  }

  return(data)
}

add_species_categories = function(data, connection = DB_IOTDB()) {
  if(has_column(data, SPECIES_CATEGORY_CODE) & !has_column(data, SPECIES_CATEGORY)) {
    CL = all_codes("SPECIES_CATEGORIES", connection)[order(+SORT)][, .(CODE, SPECIES_CATEGORY = NAME_EN)]

    data = merge(data, CL, by.x="SPECIES_CATEGORY_CODE", by.y="CODE", all.x = TRUE)
  }

  return(data)
}

add_fate_types = function(data, connection = DB_IOTDB()) {
  if(has_column(data, FATE_TYPE_CODE) & !has_column(data, FATE_TYPE)) {
    CL = all_codes("FATE_TYPES", connection)[order(+SORT)][, .(CODE, FATE_TYPE = NAME_EN)]

    data = merge(data, CL, by.x="FATE_TYPE_CODE", by.y="CODE", all.x = TRUE)
  }

  return(data)
}

add_fates = function(data, connection = DB_IOTDB()) {
  if(has_column(data, FATE_CODE) & !has_column(data, FATE)) {
    CL = all_codes("FATES", connection)[order(+SORT)][, .(CODE, FATE = NAME_EN)]

    data = merge(data, CL, by.x="FATE_CODE", by.y="CODE", all.x = TRUE)
  }

  return(data)
}

add_condition_types = function(data, connection = DB_IOTDB()) {
  if(has_column(data, CONDITION_TYPE_CODE) & !has_column(data, CONDITION_TYPE)) {
    CL = all_codes("CONDITION_TYPES", connection)[order(+SORT)][, .(CODE, CONDITION_TYPE = NAME_EN)]

    data = merge(data, CL, by.x="CONDITION_TYPE_CODE", by.y="CODE", all.x = TRUE)
  }

  return(data)
}

add_conditions = function(data, connection = DB_IOTDB()) {
  if(has_column(data, CONDITION_CODE) & !has_column(data, CONDITION)) {
    CL = all_codes("CONDITIONS", connection)[order(+SORT)][, .(CODE, CONDITION = NAME_EN)]

    data = merge(data, CL, by.x="CONDITION_CODE", by.y="CODE", all.x = TRUE)
  }

  return(data)
}

add_raisings = function(data, connection = DB_IOTDB()) {
  if(has_column(data, RAISE_CODE) & !has_column(data, RAISING)) {
    CL = all_codes("RAISINGS", connection)[order(+SORT)][, .(CODE, RAISING = NAME_EN)]

    data = merge(data, CL, by.x="RAISE_CODE", by.y="CODE", all.x = TRUE)
  }

  return(data)
}

add_measure_types = function(data, connection = DB_IOTDB()) {
  if(has_column(data, "MEASURE_TYPE_CODE") & !has_column(data, "MEASURE_TYPE")) {
    CL = all_codes("MEASURE_TYPES", connection)[order(+SORT)][, .(CODE, MEASURE_TYPE = NAME_EN)]

    data = merge(data, CL, by.x="MEASURE_TYPE_CODE", by.y="CODE", all.x = TRUE)
  }

  return(data)
}

add_missing_data_fields = function(data, connection = DB_IOTDB()) {
  records_before = nrow(data)

  data = (
    add_raisings(
      add_measure_types(
        add_conditions(
          add_condition_types(
            add_condition_type_codes(
              add_fates(
                add_fate_types(
                  add_fate_type_codes(
                    add_species_categories(
                      add_species_groups(
                        add_species_wps(
                          add_IUCN_status(
                            add_gears(
                              add_fisheries(
                                add_fishery_groups(
                                  add_fishery_types(
                                    add_fleets(
                                      add_fishing_grounds(
                                        add_quarters(
                                          data
                                        )
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
                  ),connection
                ), connection
              )
            ), connection
          ), connection
        ), connection
      ), connection
    )
  )

  records_after = nrow(data)

  if(records_after != records_before)
    stop(paste("Dataset size changed after adding all missing data fields! Num. rows before:", records_before, "- After", records_after))

  return(data)
}

drop_column_if_exists = function(data_table, column_name) {
  column_index = which(colnames(data_table) == column_name)

  if(length(column_index) == 0) return(data_table)

  data_table[[column_index]] = NULL

  return(data_table)
}
