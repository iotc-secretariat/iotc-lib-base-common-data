#' Retrieves monthly effort data from the \code{V_ROS_EF} view on \code{\link{ROS}}, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria
#'
#' @param years A vector of years the extraction should be limited to
#' @param flag_codes A vector of flag codes the extraction should be limited to
#' @param fleet_codes A vector of flag codes the extraction should be limited to
#' @param gear_codes A vector of gear codes the extraction should be limited to#'
#' @param fishery_codes A vector of fishery codes the extraction should be limited to
#' @param fishery_group_codes A vector of fishery group codes the extraction should be limited to
#' @param school_type_codes A vector of school type codes the extraction should be limited to
#' @param effort_unit_codes A vector of effort unit codes the extraction should be limited to
#' @param connection An ODBC connection to the \code{\link{ROS}} database
#' @param IOTDB_connection An ODBC connection to the \code{\link{IOTDB}} database
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_ROS_EF} matching the filtering criteria
#' @examples EF_ros(years = seq(1980, 2000, 1), fishery_group_codes = "PS", effort_unit_codes = c("SET"))
#' @export
EF_ros = function(years = NULL,
                  flag_codes = NULL,
                  fleet_codes = NULL,
                  gear_codes = NULL,
                  fishery_codes = NULL,
                  fishery_group_codes = NULL,
                  school_type_codes = NULL,
                  effort_unit_codes = NULL,
                  connection = DB_ROS(),
                  IOTDB_connection = DB_IOTDB(),
                  factorize_results = TRUE) {

  s_years       = join_values(years)
  s_flag_codes  = join_strings(flag_codes)
  s_fleet_codes = join_strings(fleet_codes)
  s_gear_codes  = join_strings(gear_codes)
  s_school_type_codes = join_strings(school_type_codes)
  s_effort_unit_codes = join_strings(effort_unit_codes)

  EF_query = paste0("SELECT * FROM [ROS_rlibs].V_EF EF WHERE ")

  if(!is.null(s_years)) EF_query = paste0(EF_query, "EF.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_flag_codes))  EF_query = paste0(EF_query, "EF.FLAG_CODE IN (", s_flag_codes, ") AND ")
  if(!is.null(s_fleet_codes)) EF_query = paste0(EF_query, "EF.FLEET_CODE IN (", s_fleet_codes, ") AND ")

  if(!is.null(s_gear_codes)) EF_query = paste0(EF_query, "EF.GEAR_CODE IN (", s_fishery_codes, ") AND ")

  if(!is.null(s_school_type_codes)) EF_query = paste0(EF_query, "EF.EFFORT_SCHOOL_TYPE_CODE IN (", s_school_type_codes, ") AND ")
  if(!is.null(s_effort_unit_codes)) EF_query = paste0(EF_query, "EF.EFFORT_UNIT_CODE IN (", s_effort_unit_codes, ") AND ")

  EF_query = paste(EF_query, "1 = 1")

  data = (decorate(query(connection, EF_query), factorize_results, connection = IOTDB_connection))

  return (
    data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
        [!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#' Retrieves monthly catch data from the  \code{V_ROS_CA} view on \code{\link{ROS}}, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria
#'
#' @param years A vector of years the extraction should be limited to
#' @param flag_codes A vector of flag codes the extraction should be limited to
#' @param fleet_codes A vector of flag codes the extraction should be limited to
#' @param gear_codes A vector of gear codes the extraction should be limited to#'
#' @param fishery_codes A vector of fishery codes the extraction should be limited to
#' @param fishery_group_codes A vector of fishery group codes the extraction should be limited to
#' @param school_type_codes A vector of school type codes the extraction should be limited to
#' @param species_codes A vector of species codes the extraction should be limited to
#' @param species_category_codes A vector of species category codes (BILLFISH, SEERFISH, NERITIC, TEMPERATE, TROPICAL, TUNAS_NEI, SHARKS, MANTAS, SEABIRDS, CETACEANS, TURTLES, OTHERS) the extraction should be limited to
#' @param species_group_codes A vector of species group codes (BILLFISH, SEERFISH, TUNAS, SHARKS, OTHERS) the extraction should be limited to
#' @param species_wp_codes A vector of species WP codes (BILL, NERI, TEMP, TROP, BYCT, UNCL) the extraction should be limited to
#' @param catch_unit_codes A vector of catch unit codes the extraction should be limited to (currently, \code{NO} and \code{MT}, as the conversion from KG is embedded)
#' @param fate_codes A vector of fate codes the extraction should be limited to
#' @param connection An ODBC connection to the \code{\link{ROS}} database
#' @param IOTDB_connection An ODBC connection to the \code{\link{IOTDB}} database
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_ROS_CA} matching the filtering criteria
#' @examples CA_ros(years = seq(1980, 2000, 1), fishery_group_codes = "PS", catch_unit_codes = c("MT"))
#' @export
CA_ros = function(years = NULL,
                  flag_codes = NULL,
                  fleet_codes = NULL,
                  gear_codes = NULL,
                  fishery_codes = NULL,
                  fishery_group_codes = NULL,
                  school_type_codes = NULL,
                  species_codes = NULL,
                  species_category_codes = NULL,
                  species_group_codes = NULL,
                  species_wp_codes = NULL,
                  catch_unit_codes = NULL,
                  fate_codes = NULL,
                  connection = DB_ROS(),
                  IOTDB_connection = DB_IOTDB(),
                  factorize_results = TRUE) {

  s_years       = join_values(years)
  s_flag_codes  = join_strings(flag_codes)
  s_fleet_codes = join_strings(fleet_codes)
  s_gear_codes  = join_strings(gear_codes)
  s_school_type_codes = join_strings(school_type_codes)
  s_species_codes     = join_strings(species_codes)
  s_species_category_codes = join_strings(species_category_codes)
  s_species_group_codes    = join_strings(species_group_codes)
  s_species_wp_codes       = join_strings(species_wp_codes)
  s_catch_unit_codes = join_strings(catch_unit_codes)
  s_fate_codes       = join_strings(fate_codes)

  CA_query = paste0("SELECT * FROM [ROS_rlibs].V_CA CA WHERE ")

  if(!is.null(s_years)) CA_query = paste0(CA_query, "CA.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_flag_codes))  CA_query = paste0(CA_query, "CA.FLAG_CODE IN (", s_flag_codes, ") AND ")
  if(!is.null(s_fleet_codes)) CA_query = paste0(CA_query, "CA.FLEET_CODE IN (", s_fleet_codes, ") AND ")

  if(!is.null(s_gear_codes)) CA_query = paste0(CA_query, "CA.GEAR_CODE IN (", s_gear_codes, ") AND ")

  if(!is.null(s_school_type_codes)) CA_query = paste0(CA_query, "CA.CATCH_SCHOOL_TYPE_CODE IN (", s_school_type_codes, ") AND ")
  if(!is.null(s_species_codes)) CA_query = paste0(CA_query, "CA.SPECIES_CODE IN (", s_species_codes, ") AND ")
  if(!is.null(s_species_category_codes)) CA_query = paste0(CA_query, "CA.SPECIES_CATEGORY_CODE IN (", s_species_category_codes, ") AND ")
  if(!is.null(s_species_group_codes)) CA_query = paste0(CA_query, "CA.SPECIES_GROUP_CODE IN (", s_species_group_codes, ") AND ")
  if(!is.null(s_species_wp_codes)) CA_query = paste0(CA_query, "CA.SPECIES_WP_CODE IN (", s_species_wp_codes, ") AND ")
  if(!is.null(s_catch_unit_codes)) CA_query = paste0(CA_query, "CA.CATCH_UNIT_CODE IN (", s_catch_unit_codes, ") AND ")
  if(!is.null(s_fate_codes)) CA_query = paste0(CA_query, "CA.FATE_CODE IN (", s_fate_codes, ") AND ")

  CA_query = paste(CA_query, "1 = 1")

  data = (decorate(query(connection, CA_query), factorize_results, connection = IOTDB_connection))

  return (
    data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
        [!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#' Retrieves monthly catch data from the  \code{V_ROS_CE} view on IOTDB, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria
#'
#' @param years A vector of years the extraction should be limited to
#' @param flag_codes A vector of flag codes the extraction should be limited to
#' @param fleet_codes A vector of flag codes the extraction should be limited to
#' @param gear_codes A vector of gear codes the extraction should be limited to#'
#' @param fishery_codes A vector of fishery codes the extraction should be limited to
#' @param fishery_group_codes A vector of fishery group codes the extraction should be limited to
#' @param school_type_codes A vector of school type codes the extraction should be limited to
#' @param species_codes A vector of species codes the extraction should be limited to
#' @param species_category_codes A vector of species category codes (BILLFISH, SEERFISH, NERITIC, TEMPERATE, TROPICAL, TUNAS_NEI, SHARKS, MANTAS, SEABIRDS, CETACEANS, TURTLES, OTHERS) the extraction should be limited to
#' @param species_group_codes A vector of species group codes (BILLFISH, SEERFISH, TUNAS, SHARKS, OTHERS) the extraction should be limited to
#' @param species_wp_codes A vector of species WP codes (BILL, NERI, TEMP, TROP, BYCT, UNCL) the extraction should be limited to
#' @param effort_unit_codes A vector of effort unit codes the extraction should be limited to
#' @param catch_unit_codes A vector of catch unit codes the extraction should be limited to (currently, \code{NO} and \code{MT}, as the conversion from KG is embedded)
#' @param fate_codes A vector of fate codes the extraction should be limited to
#' @param connection An ODBC connection to the \code{\link{ROS}} database
#' @param IOTDB_connection An ODBC connection to the \code{\link{IOTDB}} database
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_ROS_CE} matching the filtering criteria
#' @examples CE_ros(years = seq(1980, 2000, 1), effort_unit_codes = "SET", catch_unit_codes = c("MT"))
#' @export
CE_ros = function(years = NULL,
                  flag_codes = NULL,
                  fleet_codes = NULL,
                  gear_codes = NULL,
                  fishery_codes = NULL,
                  fishery_group_codes = NULL,
                  school_type_codes = NULL,
                  species_codes = NULL,
                  species_category_codes = NULL,
                  species_group_codes = NULL,
                  species_wp_codes = NULL,
                  effort_unit_codes = NULL,
                  catch_unit_codes = NULL,
                  fate_codes = NULL,
                  connection = DB_ROS(),
                  IOTDB_connection = DB_IOTDB(),
                  factorize_results = TRUE) {

  s_years       = join_values(years)
  s_flag_codes  = join_strings(flag_codes)
  s_fleet_codes = join_strings(fleet_codes)
  s_gear_codes  = join_strings(gear_codes)
  s_school_type_codes = join_strings(school_type_codes)
  s_species_codes     = join_strings(species_codes)
  s_species_category_codes = join_strings(species_category_codes)
  s_species_group_codes    = join_strings(species_group_codes)
  s_species_wp_codes       = join_strings(species_wp_codes)
  s_effort_unit_codes = join_strings(effort_unit_codes)
  s_catch_unit_codes  = join_strings(catch_unit_codes)
  s_fate_codes        = join_strings(fate_codes)

  CE_query = paste0("SELECT * FROM [ROS_rlibs].V_CE CE WHERE ")

  if(!is.null(s_years)) CE_query = paste0(CE_query, "CE.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_flag_codes))  CE_query = paste0(CE_query, "CE.FLAG_CODE IN (", s_flag_codes, ") AND ")
  if(!is.null(s_fleet_codes)) CE_query = paste0(CE_query, "CE.FLEET_CODE IN (", s_fleet_codes, ") AND ")

  if(!is.null(s_gear_codes)) CE_query = paste0(CE_query, "CE.GEAR_CODE IN (", s_gear_codes, ") AND ")

  if(!is.null(s_school_type_codes)) CE_query = paste0(CE_query, "CE.CATCH_SCHOOL_TYPE_CODE IN (", s_school_type_codes, ") AND ")
  if(!is.null(s_species_codes)) CE_query = paste0(CE_query, "CE.SPECIES_CODE IN (", s_species_codes, ") AND ")
  if(!is.null(s_species_category_codes)) CE_query = paste0(CE_query, "CE.SPECIES_CATEGORY_CODE IN (", s_species_category_codes, ") AND ")
  if(!is.null(s_species_group_codes)) CE_query = paste0(CE_query, "CE.SPECIES_GROUP_CODE IN (", s_species_group_codes, ") AND ")
  if(!is.null(s_species_wp_codes)) CE_query = paste0(CE_query, "CE.SPECIES_WP_CODE IN (", s_species_wp_codes, ") AND ")
  if(!is.null(s_effort_unit_codes)) CE_query = paste0(CE_query, "CE.EFFORT_UNIT_CODE IN (", s_effort_unit_codes, ") AND ")
  if(!is.null(s_catch_unit_codes)) CE_query = paste0(CE_query, "CE.CATCH_UNIT_CODE IN (", s_catch_unit_codes, ") AND ")
  if(!is.null(s_fate_codes)) CE_query = paste0(CE_query, "CE.FATE_CODE IN (", s_fate_codes, ") AND ")

  CE_query = paste(CE_query, "1 = 1")

  data = (decorate(query(connection, CE_query), factorize_results, connection = IOTDB_connection))

  return (
    data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
        [!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#' Retrieves monthly interaction data from the  \code{V_ROS_IN} view on \code{\link{ROS_ANALYSIS}}, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria
#'
#' @param years A vector of years the extraction should be limited to
#' @param flag_codes A vector of flag codes the extraction should be limited to
#' @param fleet_codes A vector of flag codes the extraction should be limited to
#' @param gear_codes A vector of gear codes the extraction should be limited to#'
#' @param fishery_codes A vector of fishery codes the extraction should be limited to
#' @param fishery_group_codes A vector of fishery group codes the extraction should be limited to
#' @param school_type_codes A vector of school type codes the extraction should be limited to
#' @param species_codes A vector of species codes the extraction should be limited to
#' @param species_category_codes A vector of species category codes (BILLFISH, SEERFISH, NERITIC, TEMPERATE, TROPICAL, TUNAS_NEI, SHARKS, MANTAS, SEABIRDS, CETACEANS, TURTLES, OTHERS) the extraction should be limited to
#' @param species_group_codes A vector of species group codes (BILLFISH, SEERFISH, TUNAS, SHARKS, OTHERS) the extraction should be limited to
#' @param species_wp_codes A vector of species WP codes (BILL, NERI, TEMP, TROP, BYCT, UNCL) the extraction should be limited to
#' @param fate_codes A vector of fate codes the extraction should be limited to
#' @param condition_codes A vector of condition codes the extraction should be limited to
#' @param connection An ODBC connection to the \code{\link{ROS}} database
#' @param IOTDB_connection An ODBC connection to the \code{\link{IOTDB}} database to support retrieval of reference data
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_ROS_CA} matching the filtering criteria
#' @examples IN_ros(years = seq(1980, 2000, 1), fishery_group_codes = "PS", catch_unit_codes = c("MT"))
#' @export
IN_ros = function(years = NULL,
                  flag_codes = NULL,
                  fleet_codes = NULL,
                  gear_codes = NULL,
                  fishery_codes = NULL,
                  fishery_group_codes = NULL,
                  school_type_codes = NULL,
                  species_codes = NULL,
                  species_category_codes = NULL,
                  species_group_codes = NULL,
                  species_wp_codes = NULL,
                  fate_codes = NULL,
                  condition_codes = NULL,
                  connection = DB_ROS(),
                  IOTDB_connection = DB_IOTDB(),
                  factorize_results = TRUE) {

  s_years       = join_values(years)
  s_flag_codes  = join_strings(flag_codes)
  s_fleet_codes = join_strings(fleet_codes)
  s_gear_codes  = join_strings(gear_codes)
  s_school_type_codes = join_strings(school_type_codes)
  s_species_codes     = join_strings(species_codes)
  s_species_category_codes = join_strings(species_category_codes)
  s_species_group_codes    = join_strings(species_group_codes)
  s_species_wp_codes       = join_strings(species_wp_codes)
  s_fate_codes      = join_strings(fate_codes)
  s_condition_codes = join_strings(condition_codes)

  IN_query = paste0("SELECT * FROM [ROS_rlibs].V_IN RIN WHERE ")

  if(!is.null(s_years)) IN_query = paste0(IN_query, "RIN.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_flag_codes))  IN_query = paste0(IN_query, "RIN.FLAG_CODE IN (", s_flag_codes, ") AND ")
  if(!is.null(s_fleet_codes)) IN_query = paste0(IN_query, "RIN.FLEET_CODE IN (", s_fleet_codes, ") AND ")

  if(!is.null(s_gear_codes)) IN_query = paste0(IN_query, "RIN.GEAR_CODE IN (", s_gear_codes, ") AND ")

  if(!is.null(s_school_type_codes)) IN_query = paste0(IN_query, "RIN.CATCH_SCHOOL_TYPE_CODE IN (", s_school_type_codes, ") AND ")
  if(!is.null(s_species_codes)) IN_query = paste0(IN_query, "RIN.SPECIES_CODE IN (", s_species_codes, ") AND ")
  if(!is.null(s_species_category_codes)) IN_query = paste0(IN_query, "RIN.SPECIES_CATEGORY_CODE IN (", s_species_category_codes, ") AND ")
  if(!is.null(s_species_group_codes)) IN_query = paste0(IN_query, "RIN.SPECIES_GROUP_CODE IN (", s_species_group_codes, ") AND ")
  if(!is.null(s_species_wp_codes)) IN_query = paste0(IN_query, "RIN.SPECIES_WP_CODE IN (", s_species_wp_codes, ") AND ")
  if(!is.null(s_fate_codes)) IN_query = paste0(IN_query, "RIN.FATE_CODE IN (", s_fate_codes, ") AND ")
  if(!is.null(s_condition_codes)) IN_query = paste0(IN_query, "RIN.CONDITION_CODE IN (", s_condition_codes, ") AND ")

  IN_query = paste(IN_query, "1 = 1")

  data = (decorate(query(connection, IN_query), factorize_results, connection = IOTDB_connection))

  return (
    data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
    [!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#' Retrieves ROS monthly sample data from the \code{[ROS_rlibs].[V_SA} view on \code{\link{ROS_ANALYSIS}} DB, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria
#'
#' @param years A vector of years the extraction should be limited to
#' @param flag_codes A vector of flag codes the extraction should be limited to
#' @param fleet_codes A vector of flag codes the extraction should be limited to
#' @param gear_codes A vector of gear codes the extraction should be limited to#'
#' @param fishery_codes A vector of fishery codes the extraction should be limited to
#' @param fishery_group_codes A vector of fishery group codes the extraction should be limited to
#' @param school_type_codes A vector of school type codes the extraction should be limited to
#' @param species_codes A vector of species codes the extraction should be limited to
#' @param species_category_codes A vector of species category codes (BILLFISH, SEERFISH, NERITIC, TEMPERATE, TROPICAL, TUNAS_NEI, SHARKS, MANTAS, SEABIRDS, CETACEANS, TURTLES, OTHERS) the extraction should be limited to
#' @param species_group_codes A vector of species group codes (BILLFISH, SEERFISH, TUNAS, SHARKS, OTHERS) the extraction should be limited to
#' @param species_wp_codes A vector of species WP codes (BILL, NERI, TEMP, TROP, BYCT, UNCL) the extraction should be limited to
#' @param measure_type_codes A vector of measure type codes the extraction should be limited to
#' @param connection An ODBC connection to the \code{\link{ROS}} database
#' @param IOTDB_connection An ODBC connection to the \code{\link{IOTDB}} database to support retrieval of reference data
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_ROS_SA} matching the filtering criteria
#' @examples SF_ros(years = seq(1980, 2000, 1), measure_type_codes = "FL", species_wp_codes = "NERI")
#' @export
SA_ros = function(years = NULL,
                  flag_codes = NULL,
                  fleet_codes = NULL,
                  gear_codes = NULL,
                  fishery_codes = NULL,
                  fishery_group_codes = NULL,
                  school_type_codes = NULL,
                  species_codes = NULL,
                  species_category_codes = NULL,
                  species_group_codes = NULL,
                  species_wp_codes = NULL,
                  measure_type_codes = NULL,
                  connection = DB_ROS(),
                  IOTDB_connection = DB_IOTDB(),
                  factorize_results = TRUE) {

  s_years       = join_values(years)
  s_flag_codes  = join_strings(flag_codes)
  s_fleet_codes = join_strings(fleet_codes)
  s_gear_codes  = join_strings(gear_codes)
  s_school_type_codes = join_strings(school_type_codes)
  s_species_codes     = join_strings(species_codes)
  s_species_category_codes = join_strings(species_category_codes)
  s_species_group_codes    = join_strings(species_group_codes)
  s_species_wp_codes       = join_strings(species_wp_codes)
  s_measure_type_codes = join_strings(measure_type_codes)

  SA_query = paste0("SELECT * FROM [ROS_rlibs].V_SA SA WHERE ")

  if(!is.null(s_years)) SA_query = paste0(SA_query, "SA.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_flag_codes))  SA_query = paste0(SA_query, "SA.FLAG_CODE IN (", s_flag_codes, ") AND ")
  if(!is.null(s_fleet_codes)) SA_query = paste0(SA_query, "SA.FLEET_CODE IN (", s_fleet_codes, ") AND ")

  if(!is.null(s_gear_codes)) SA_query = paste0(SA_query, "SA.GEAR_CODE IN (", s_gear_codes, ") AND ")

  if(!is.null(s_school_type_codes)) SA_query = paste0(SA_query, "SA.SCHOOL_TYPE_CODE IN (", s_school_type_codes, ") AND ")
  if(!is.null(s_species_codes)) SA_query = paste0(SA_query, "SA.SPECIES_CODE IN (", s_species_codes, ") AND ")
  if(!is.null(s_species_category_codes)) SA_query = paste0(SA_query, "SA.SPECIES_CATEGORY_CODE IN (", s_species_category_codes, ") AND ")
  if(!is.null(s_species_group_codes)) SA_query = paste0(SA_query, "SA.SPECIES_GROUP_CODE IN (", s_species_group_codes, ") AND ")
  if(!is.null(s_species_wp_codes)) SA_query = paste0(SA_query, "SA.SPECIES_WP_CODE IN (", s_species_wp_codes, ") AND ")
  if(!is.null(s_measure_type_codes)) SA_query = paste0(SA_query, "SA.MEASURE_TYPE_CODE IN (", s_measure_type_codes, ") AND ")

  SA_query = paste(SA_query, "1 = 1")

  data = (decorate(query(connection, SA_query), factorize_results, connection = IOTDB_connection))

  return (
    data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
    [!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#' Retrieves ROS monthly size-frequency data from the \code{[ROS_rlibs].[V_SF} view on \code{\link{ROS_ANALYSIS}}, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria
#'
#' @param years A vector of years the extraction should be limited to
#' @param flag_codes A vector of flag codes the extraction should be limited to
#' @param fleet_codes A vector of flag codes the extraction should be limited to
#' @param gear_codes A vector of gear codes the extraction should be limited to#'
#' @param fishery_codes A vector of fishery codes the extraction should be limited to
#' @param fishery_group_codes A vector of fishery group codes the extraction should be limited to
#' @param school_type_codes A vector of school type codes the extraction should be limited to
#' @param species_codes A vector of species codes the extraction should be limited to
#' @param species_category_codes A vector of species category codes (BILLFISH, SEERFISH, NERITIC, TEMPERATE, TROPICAL, TUNAS_NEI, SHARKS, MANTAS, SEABIRDS, CETACEANS, TURTLES, OTHERS) the extraction should be limited to
#' @param species_group_codes A vector of species group codes (BILLFISH, SEERFISH, TUNAS, SHARKS, OTHERS) the extraction should be limited to
#' @param species_wp_codes A vector of species WP codes (BILL, NERI, TEMP, TROP, BYCT, UNCL) the extraction should be limited to
#' @param measure_type_codes A vector of measure type codes the extraction should be limited to
#' @param sex_codes A vector of sex codes the extraction should be limited to
#' @param connection An ODBC connection to the \code{\link{ROS}} database
#' @param IOTDB_connection An ODBC connection to the \code{\link{IOTDB}} database to support retrieval of reference data
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_ROS_SF} matching the filtering criteria
#' @examples SF_raw(years = seq(1980, 2000, 1), measure_type_codes = "FL", species_wp_codes = "NERI")
#' @export
SF_ros = function(years = NULL,
                  flag_codes = NULL,
                  fleet_codes = NULL,
                  gear_codes = NULL,
                  fishery_codes = NULL,
                  fishery_group_codes = NULL,
                  school_type_codes = NULL,
                  species_codes = NULL,
                  species_category_codes = NULL,
                  species_group_codes = NULL,
                  species_wp_codes = NULL,
                  measure_type_codes = NULL,
                  sex_codes = NULL,
                  connection = DB_ROS(),
                  IOTDB_connection = DB_IOTDB(),
                  factorize_results = TRUE) {

  s_years       = join_values(years)
  s_flag_codes  = join_strings(flag_codes)
  s_fleet_codes = join_strings(fleet_codes)
  s_gear_codes  = join_strings(gear_codes)
  s_school_type_codes = join_strings(school_type_codes)
  s_species_codes     = join_strings(species_codes)
  s_species_category_codes = join_strings(species_category_codes)
  s_species_group_codes    = join_strings(species_group_codes)
  s_species_wp_codes       = join_strings(species_wp_codes)
  s_measure_type_codes = join_strings(measure_type_codes)
  s_sex_codes          = join_strings(sex_codes)

  SF_query = paste0("SELECT * FROM [ROS_rlibs].V_SF SF WHERE ")

  if(!is.null(s_years)) SF_query = paste0(SF_query, "SF.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_flag_codes))  SF_query = paste0(SF_query, "SF.FLAG_CODE IN (", s_flag_codes, ") AND ")
  if(!is.null(s_fleet_codes)) SF_query = paste0(SF_query, "SF.FLEET_CODE IN (", s_fleet_codes, ") AND ")

  if(!is.null(s_gear_codes)) SF_query = paste0(SF_query, "SF.GEAR_CODE IN (", s_gear_codes, ") AND ")

  if(!is.null(s_school_type_codes)) SF_query = paste0(SF_query, "SF.SCHOOL_TYPE_CODE IN (", s_school_type_codes, ") AND ")
  if(!is.null(s_species_codes)) SF_query = paste0(SF_query, "SF.SPECIES_CODE IN (", s_species_codes, ") AND ")
  if(!is.null(s_species_category_codes)) SF_query = paste0(SF_query, "SF.SPECIES_CATEGORY_CODE IN (", s_species_category_codes, ") AND ")
  if(!is.null(s_species_group_codes)) SF_query = paste0(SF_query, "SF.SPECIES_GROUP_CODE IN (", s_species_group_codes, ") AND ")
  if(!is.null(s_species_wp_codes)) SF_query = paste0(SF_query, "SF.SPECIES_WP_CODE IN (", s_species_wp_codes, ") AND ")
  if(!is.null(s_measure_type_codes)) SF_query = paste0(SF_query, "SF.MEASURE_TYPE_CODE IN (", s_measure_type_codes, ") AND ")
  if(!is.null(s_sex_codes)) SF_query = paste0(SF_query, "SF.SEX_CODE IN (", s_sex_codes, ") AND ")

  SF_query = paste(SF_query, "1 = 1")

  data = (decorate(query(connection, SF_query), factorize_results, connection = IOTDB_connection))

  return (
    data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
    [!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#' Retrieves ROS monthly length-weight data from the \code{[ROS_rlibs].[V_LW} view on \code{\link{ROS}}, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria
#'
#' @param years A vector of years the extraction should be limited to
#' @param flag_codes A vector of flag codes the extraction should be limited to
#' @param fleet_codes A vector of flag codes the extraction should be limited to
#' @param gear_codes A vector of gear codes the extraction should be limited to#'
#' @param fishery_codes A vector of fishery codes the extraction should be limited to
#' @param fishery_group_codes A vector of fishery group codes the extraction should be limited to
#' @param school_type_codes A vector of school type codes the extraction should be limited to
#' @param species_codes A vector of species codes the extraction should be limited to
#' @param species_category_codes A vector of species category codes (BILLFISH, SEERFISH, NERITIC, TEMPERATE, TROPICAL, TUNAS_NEI, SHARKS, MANTAS, SEABIRDS, CETACEANS, TURTLES, OTHERS) the extraction should be limited to
#' @param species_group_codes A vector of species group codes (BILLFISH, SEERFISH, TUNAS, SHARKS, OTHERS) the extraction should be limited to
#' @param species_wp_codes A vector of species WP codes (BILL, NERI, TEMP, TROP, BYCT, UNCL) the extraction should be limited to
#' @param length_measure_type_codes A vector of length measure type codes the extraction should be limited to
#' @param weight_measure_type_codes A vector of length measure type codes the extraction should be limited to
#' @param sex_codes A vector of sex codes the extraction should be limited to
#' @param connection An ODBC connection to the \code{\link{ROS}} database
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @param IOTDB_connection An ODBC connection to the \code{\link{IOTDB}} database to support retrieval of reference data
#' @return a data frame with the records from \code{.V_ROS_LW} matching the filtering criteria
#' @examples SF_raw(years = seq(1980, 2000, 1), measure_type_codes = "FL", species_wp_codes = "NERI")
#' @export
LW_ros = function(years = NULL,
                  flag_codes = NULL,
                  fleet_codes = NULL,
                  gear_codes = NULL,
                  fishery_codes = NULL,
                  fishery_group_codes = NULL,
                  school_type_codes = NULL,
                  species_codes = NULL,
                  species_category_codes = NULL,
                  species_group_codes = NULL,
                  species_wp_codes = NULL,
                  length_measure_type_codes = NULL,
                  weight_measure_type_codes = NULL,
                  sex_codes = NULL,
                  connection = DB_ROS(),
                  IOTDB_connection = DB_IOTDB(),
                  factorize_results = TRUE) {

  s_years       = join_values(years)
  s_flag_codes  = join_strings(flag_codes)
  s_fleet_codes = join_strings(fleet_codes)
  s_gear_codes  = join_strings(gear_codes)
  s_school_type_codes = join_strings(school_type_codes)
  s_species_codes     = join_strings(species_codes)
  s_species_category_codes = join_strings(species_category_codes)
  s_species_group_codes    = join_strings(species_group_codes)
  s_species_wp_codes       = join_strings(species_wp_codes)
  s_length_type_codes = join_strings(length_measure_type_codes)
  s_weight_type_codes = join_strings(weight_measure_type_codes)
  s_sex_codes          = join_strings(sex_codes)

  LW_query = paste0("SELECT * FROM [ROS_analysis].V_ROS_LW LW WHERE ")

  if(!is.null(s_years)) LW_query = paste0(LW_query, "LW.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_flag_codes))  LW_query = paste0(LW_query, "LW.FLAG_CODE IN (", s_flag_codes, ") AND ")
  if(!is.null(s_fleet_codes)) LW_query = paste0(LW_query, "LW.FLEET_CODE IN (", s_fleet_codes, ") AND ")

  if(!is.null(s_gear_codes)) LW_query = paste0(LW_query, "LW.GEAR_CODE IN (", s_gear_codes, ") AND ")

  if(!is.null(s_school_type_codes)) LW_query = paste0(LW_query, "LW.SCHOOL_TYPE_CODE IN (", s_school_type_codes, ") AND ")
  if(!is.null(s_species_codes)) LW_query = paste0(LW_query, "LW.SPECIES_CODE IN (", s_species_codes, ") AND ")
  if(!is.null(s_species_category_codes)) LW_query = paste0(LW_query, "LW.SPECIES_CATEGORY_CODE IN (", s_species_category_codes, ") AND ")
  if(!is.null(s_species_group_codes)) LW_query = paste0(LW_query, "LW.SPECIES_GROUP_CODE IN (", s_species_group_codes, ") AND ")
  if(!is.null(s_species_wp_codes)) LW_query = paste0(LW_query, "LW.SPECIES_WP_CODE IN (", s_species_wp_codes, ") AND ")
  if(!is.null(s_length_type_codes)) LW_query = paste0(LW_query, "LW.LENGTH_MEASURE_TYPE_CODE IN (", s_length_type_codes, ") AND ")
  if(!is.null(s_weight_type_codes)) LW_query = paste0(LW_query, "LW.WEIGHT_MEASURE_TYPE_CODE IN (", s_weight_type_codes, ") AND ")
  if(!is.null(s_sex_codes)) LW_query = paste0(LW_query, "LW.SEX_CODE IN (", s_sex_codes, ") AND ")

  LW_query = paste(LW_query, "1 = 1")

  data = (decorate(query(connection, LW_query), factorize_results, connection = IOTDB_connection))

  return (
    data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
    [!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#' Retrieves ROS raw set data (including the type of event among \code{SETTING} and \code{HAULING}, the starting time and coordinates, the ending time and coordinates and the effort measured for the event)
#' @param years A vector of years the extraction should be limited to
#' @param date_from A starting date (as '\code{yyyy-MM-dd}') the extraction should be limited to
#' @param date_to An ending date (as '\code{yyyy-MM-dd}') the extraction should be limited to
#' @param flag_codes A vector of flag codes the extraction should be limited to
#' @param fleet_codes A vector of flag codes the extraction should be limited to
#' @param gear_codes A vector of gear codes the extraction should be limited to
#' @param fishery_codes A vector of fishery codes the extraction should be limited to
#' @param fishery_group_codes A vector of fishery group codes the extraction should be limited to
#' @param school_type_codes A vector of school type codes the extraction should be limited to
#' @param species_codes A vector of species codes the extraction should be limited to
#' @param species_category_codes A vector of species category codes (BILLFISH, SEERFISH, NERITIC, TEMPERATE, TROPICAL, TUNAS_NEI, SHARKS, MANTAS, SEABIRDS, CETACEANS, TURTLES, OTHERS) the extraction should be limited to
#' @param species_group_codes A vector of species group codes (BILLFISH, SEERFISH, TUNAS, SHARKS, OTHERS) the extraction should be limited to
#' @param species_wp_codes A vector of species WP codes (BILL, NERI, TEMP, TROP, BYCT, UNCL) the extraction should be limited to
#' @param length_measure_type_codes A vector of length measure type codes the extraction should be limited to
#' @param weight_measure_type_codes A vector of length measure type codes the extraction should be limited to
#' @param sex_codes A vector of sex codes the extraction should be limited to
#' @param connection An ODBC connection to the \code{\link{ROS}} database
#' @param IOTDB_connection An ODBC connection to the \code{\link{IOTDB}} database to support retrieval of reference data
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_SETS_RAW} matching the filtering criteria
#' @export
SETS_raw_ros = function(years = NULL,
                        date_from = NULL,
                        date_to = NULL,
                        flag_codes = NULL,
                        fleet_codes = NULL,
                        gear_codes = NULL,
                        fishery_codes = NULL,
                        fishery_group_codes = NULL,
                        event_type_codes = NULL,
                        connection = DB_ROS(),
                        IOTDB_connection = DB_IOTDB(),
                        factorize_results = TRUE) {

  s_years       = join_values(years)
  s_flag_codes  = join_strings(flag_codes)
  s_fleet_codes = join_strings(fleet_codes)
  s_gear_codes  = join_strings(gear_codes)
  s_event_type_codes = join_strings(event_type_codes)

  RS_query = paste0("SELECT * FROM [dbo].V_SETS_RAW RS WHERE ")

  if(!is.null(s_years)) RS_query = paste0(RS_query, "(YEAR(RS.START_TIME) IN (", s_years, ") OR YEAR(RS.END_TIME) IN (", s_years, ")) AND ")

  if(!is.null(date_from)) RS_query = paste0(RS_query, "(RS.START_TIME >= '", date_from, "' OR RS.END_TIME <= '", date_from, "')) AND ")
  if(!is.null(date_to)) RS_query = paste0(RS_query, "(RS.START_TIME <= '", date_to, "' OR RS.END_TIME >= '", date_to, "')) AND ")

  if(!is.null(s_flag_codes))  RS_query = paste0(RS_query, "RS.FLAG_CODE IN (", s_flag_codes, ") AND ")
  if(!is.null(s_fleet_codes)) RS_query = paste0(RS_query, "RS.FLEET_CODE IN (", s_fleet_codes, ") AND ")

  if(!is.null(s_gear_codes)) RS_query = paste0(RS_query, "RS.GEAR_CODE IN (", s_gear_codes, ") AND ")

  if(!is.null(s_event_type_codes)) RS_query = paste0(RS_query, "RS.EVENT_TYPE_CODE IN (", s_event_type_codes, ") AND ")

  RS_query = paste(RS_query, "1 = 1")

  data = (decorate(query(connection, RS_query), factorize_results, connection = IOTDB_connection))

  return (
    data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
    [!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#'Alias for \code{\link{EF_ros}}
#'@export
ros.EF = EF_ros

#'Alias for \code{\link{EF_ros}}
#'@export
EF.ros = EF_ros

#'Alias for \code{\link{CA_ros}}
#'@export
ros.CA = CA_ros

#'Alias for \code{\link{CA_ros}}
#'@export
CA.ros = CA_ros

#'Alias for \code{\link{CE_ros}}
#'@export
ros.CE = CE_ros

#'Alias for \code{\link{CE_ros}}
#'@export
CE.ros = CE_ros

#'Alias for \code{\link{IN_ros}}
#'@export
ros.IN = IN_ros

#'Alias for \code{\link{IN_ros}}
#'@export
IN.ros = IN_ros

#'Alias for \code{\link{SA_ros}}
#'@export
ros.SA = SA_ros

#'Alias for \code{\link{SA_ros}}
#'@export
SA.ros = SA_ros

#'Alias for \code{\link{SF_ros}}
#'@export
ros.SF = SF_ros

#'Alias for \code{\link{SF_ros}}
#'@export
SF.ros = SF_ros

#'Alias for \code{\link{LW_ros}}
#'@export
ros.LW = LW_ros

#'Alias for \code{\link{LW_ros}}
#'@export
LW.ros = LW_ros

#'Alias for \code{\link{SETS_raw_ros}}
#'@export
ros.raw.SETS = SETS_raw_ros

#'Alias for \code{\link{SETS_raw_ros}}
#'@export
SETS.raw.ros = SETS_raw_ros
