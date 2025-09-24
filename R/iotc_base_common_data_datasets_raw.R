#' Retrieves *raw* yearly nominal catch data from the 'standard' \code{V_LEGACY_NC_STD} or 'official' \code{V_LEGACY_NC_OFF} view on IOTDB, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria.
#'
#' The 'standard' version includes the 'FLAG_CODE' column as a combination of the codes from both
#' the flag country and the reporting country for the record, and the 'FLEET_CODE' column as it derives
#' from the combination of FLAG_CODE and fishing gear mapped by the \code{CountryStratVsFleet}
#' (which turns several flag + reporting country into generic fleets such as \code{NEICE}, \code{NEIFR}, \code{NEIPS} and similar).
#' The 'official' version only includes the 'FLEET_CODE' column determined as indicated above.
#'
#' @param use_official A flag that tells whether using the official (if TRUE) or standard (if FALSE) view
#' @param years A vector of years the extraction should be limited to
#' @param flag_codes A vector of flag codes the extraction should be limited to
#' @param fleet_codes A vector of flag codes the extraction should be limited to
#' @param gear_codes A vector of gear codes the extraction should be limited to#'
#' @param fishery_codes A vector of fishery codes the extraction should be limited to
#' @param fishery_group_codes A vector of fishery group codes the extraction should be limited to
#' @param species_codes A vector of species codes the extraction should be limited to
#' @param species_category_codes A vector of species category codes (BILLFISH, SEERFISH, NERITIC, TEMPERATE, TROPICAL, TUNAS_NEI, SHARKS, MANTAS, SEABIRDS, CETACEANS, TURTLES, OTHERS) the extraction should be limited to
#' @param species_group_codes A vector of species group codes (BILLFISH, SEERFISH, TUNAS, SHARKS, OTHERS) the extraction should be limited to
#' @param species_wp_codes A vector of species WP codes (BILL, NERI, TEMP, TROP, BYCT, UNCL) the extraction should be limited to
#' @param connection An ODBC connection to the \code{\link{IOTDB}} database
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_LEGACY_NC} matching the filtering criteria
#' @examples NC_raw(years = seq(1980, 2000, 1), species_codes = c("ALB", "SBF"))
#' @examples NC_raw(use_official = FALSE, years = seq(1980, 2000, 1), species_codes = c("ALB", "SBF"))
#' @export
NC_raw = function(use_official = TRUE,
                  years = NULL,
                  flag_codes = NULL,
                  fleet_codes = NULL,
                  gear_codes = NULL,
                  fishery_codes = NULL,
                  fishery_group_codes = NULL,
                  species_codes = NULL,
                  species_category_codes = NULL,
                  species_group_codes = NULL,
                  species_wp_codes = NULL,
                  connection = iotc.core.db.connections::getDefaultDBIHandler()(),
                  factorize_results = TRUE) {

  s_years         = join_values(years)
  s_flag_codes    = join_strings(flag_codes)
  s_fleet_codes   = join_strings(fleet_codes)
  s_gear_codes    = join_strings(gear_codes)
  s_species_codes = join_strings(species_codes)
  s_species_category_codes = join_strings(species_category_codes)
  s_species_group_codes    = join_strings(species_group_codes)
  s_species_wp_codes       = join_strings(species_wp_codes)

  NC_query = paste0("SELECT * FROM [meta].V_LEGACY_NC_", ifelse(use_official, "OFF", "STD"), " NC WHERE ")

  if(!is.null(s_years)) NC_query = paste0(NC_query, "NC.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_flag_codes) & use_official) {
    stop("You can't provide flag codes to filter 'official' datasets: use fleet codes instead")
  }

  if(!is.null(s_flag_codes))  NC_query = paste0(NC_query, "NC.FLAG_CODE IN (", s_flag_codes, ") AND ")
  if(!is.null(s_fleet_codes)) NC_query = paste0(NC_query, "NC.FLEET_CODE IN (", s_fleet_codes, ") AND ")

  if(!is.null(s_gear_codes)) NC_query = paste0(NC_query, "NC.GEAR_CODE IN (", s_gear_codes, ") AND ")

  if(!is.null(s_species_codes)) NC_query = paste0(NC_query, "NC.SPECIES_CODE IN (", s_species_codes, ") AND ")
  if(!is.null(s_species_category_codes)) NC_query = paste0(NC_query, "NC.SPECIES_CATEGORY_CODE IN (", s_species_category_codes, ") AND ")
  if(!is.null(s_species_group_codes)) NC_query = paste0(NC_query, "NC.SPECIES_GROUP_CODE IN (", s_species_group_codes, ") AND ")
  if(!is.null(s_species_wp_codes)) NC_query = paste0(NC_query, "NC.SPECIES_WP_CODE IN (", s_species_wp_codes, ") AND ")

  NC_query = paste(NC_query, "1 = 1")

  data = query(connection, NC_query)

  if(!"QUARTER" %in% colnames(data)) data$QUARTER = as.integer(NA)

  data = decorate(data, factorize_results, connection = connection)

  #Filters by fishery code / fishery group code, if required (the two data fields are not available within the query)
  data = data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
  data = data[!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]

  # FFiorellato: WHY? I'm supposed to know it, but it goes beyond my (current) understanding
  if(factorize_results) data = factorize_fishing_grounds(data, connection = connection)

  return(data)
}

#' Retrieves *raw* monthly effort data from the 'standard' \code{V_LEGACY_EF_STD} or 'official' \code{V_LEGACY_EF_OFF} view on IOTDB, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria
#'
#' The 'standard' version includes the 'FLAG_CODE' column as a combination of the codes from both
#' the flag country and the reporting country for the record, and the 'FLEET_CODE' column as it derives
#' from the combination of FLAG_CODE and fishing gear mapped by the \code{CountryStratVsFleet}
#' (which turns several flag + reporting country into generic fleets such as \code{NEICE}, \code{NEIFR}, \code{NEIPS} and similar).
#' The 'official' version only includes the 'FLEET_CODE' column determined as indicated above.
#'
#' @param use_official A flag that tells whether using the official (if TRUE) or standard (if FALSE) view
#' @param years A vector of years the extraction should be limited to
#' @param flag_codes A vector of flag codes the extraction should be limited to
#' @param fleet_codes A vector of flag codes the extraction should be limited to
#' @param gear_codes A vector of gear codes the extraction should be limited to#'
#' @param fishery_codes A vector of fishery codes the extraction should be limited to
#' @param fishery_group_codes A vector of fishery group codes the extraction should be limited to
#' @param school_type_codes A vector of school type codes the extraction should be limited to
#' @param effort_unit_codes A vector of effort unit codes the extraction should be limited to
#' @param connection An ODBC connection to the \code{\link{IOTDB}} database
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_LEGACY_EF} matching the filtering criteria
#' @examples EF_raw(years = seq(1980, 2000, 1), fishery_group_codes = "PS", effort_unit_codes = c("FHOURS", "FDAYS"))
#' @examples EF_raw(use_official = FALSE, years = seq(1980, 2000, 1), fishery_group_codes = "PS", effort_unit_codes = c("FHOURS", "FDAYS"))
#' @export
EF_raw = function(use_official = TRUE,
                  years = NULL,
                  flag_codes = NULL,
                  fleet_codes = NULL,
                  gear_codes = NULL,
                  fishery_codes = NULL,
                  fishery_group_codes = NULL,
                  school_type_codes = NULL,
                  effort_unit_codes = NULL,
                  connection = iotc.core.db.connections::getDefaultDBIHandler()(),
                  factorize_results = TRUE) {

  s_years       = join_values(years)
  s_flag_codes  = join_strings(flag_codes)
  s_fleet_codes = join_strings(fleet_codes)
  s_gear_codes  = join_strings(gear_codes)
  s_school_type_codes = join_strings(school_type_codes)
  s_effort_unit_codes = join_strings(effort_unit_codes)

  EF_query = paste0("SELECT * FROM [meta].V_LEGACY_EF_", ifelse(use_official, "OFF", "STD"), " EF WHERE ")

  if(!is.null(s_years)) EF_query = paste0(EF_query, "EF.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_flag_codes) & use_official) {
    stop("You can't provide flag codes to filter 'official' datasets: use fleet codes instead")
  }

  if(!is.null(s_flag_codes))  EF_query = paste0(EF_query, "EF.FLAG_CODE IN (", s_flag_codes, ") AND ")
  if(!is.null(s_fleet_codes)) EF_query = paste0(EF_query, "EF.FLEET_CODE IN (", s_fleet_codes, ") AND ")

  if(!is.null(s_gear_codes)) EF_query = paste0(EF_query, "EF.GEAR_CODE IN (", s_gear_codes, ") AND ")

  if(!is.null(s_school_type_codes)) EF_query = paste0(EF_query, "EF.EFFORT_SCHOOL_TYPE_CODE IN (", s_school_type_codes, ") AND ")
  if(!is.null(s_effort_unit_codes)) EF_query = paste0(EF_query, "EF.EFFORT_UNIT_CODE IN (", s_effort_unit_codes, ") AND ")

  EF_query = paste(EF_query, "1 = 1")

  data = (decorate(query(connection, EF_query), factorize_results, connection = connection))

  return (
    data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
        [!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#' Retrieves *raw* monthly catch data from the 'standard' \code{V_LEGACY_CA_STD} or 'official' \code{V_LEGACY_CA_OFF} view on IOTDB, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria
#'
#' The 'standard' version includes the 'FLAG_CODE' column as a combination of the codes from both
#' the flag country and the reporting country for the record, and the 'FLEET_CODE' column as it derives
#' from the combination of FLAG_CODE and fishing gear mapped by the \code{CountryStratVsFleet}
#' (which turns several flag + reporting country into generic fleets such as \code{NEICE}, \code{NEIFR}, \code{NEIPS} and similar).
#' The 'official' version only includes the 'FLEET_CODE' column determined as indicated above.
#'
#' @param use_official A flag that tells whether using the official (if TRUE) or standard (if FALSE) view
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
#' @param connection An ODBC connection to the \code{\link{IOTDB}} database
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_LEGACY_CA} matching the filtering criteria
#' @examples CA_raw(years = seq(1980, 2000, 1), fishery_group_codes = "LL", catch_unit_codes = c("MT"))
#' @export
CA_raw = function(use_official = TRUE,
                  years = NULL,
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
                  connection = iotc.core.db.connections::getDefaultDBIHandler()(),
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

  CA_query = paste0("SELECT * FROM [meta].V_LEGACY_CA_", ifelse(use_official, "OFF", "STD"), " CA WHERE ")

  if(!is.null(s_years)) CA_query = paste0(CA_query, "CA.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_flag_codes) & use_official) {
    stop("You can't provide flag codes to filter 'official' datasets: use fleet codes instead")
  }

  if(!is.null(s_flag_codes))  CA_query = paste0(CA_query, "CA.FLAG_CODE IN (", s_flag_codes, ") AND ")
  if(!is.null(s_fleet_codes)) CA_query = paste0(CA_query, "CA.FLEET_CODE IN (", s_fleet_codes, ") AND ")

  if(!is.null(s_gear_codes)) CA_query = paste0(CA_query, "CA.GEAR_CODE IN (", s_gear_codes, ") AND ")

  if(!is.null(s_school_type_codes)) CA_query = paste0(CA_query, "CA.CATCH_SCHOOL_TYPE_CODE IN (", s_school_type_codes, ") AND ")
  if(!is.null(s_species_codes)) CA_query = paste0(CA_query, "CA.SPECIES_CODE IN (", s_species_codes, ") AND ")
  if(!is.null(s_species_category_codes)) CA_query = paste0(CA_query, "CA.SPECIES_CATEGORY_CODE IN (", s_species_category_codes, ") AND ")
  if(!is.null(s_species_group_codes)) CA_query = paste0(CA_query, "CA.SPECIES_GROUP_CODE IN (", s_species_group_codes, ") AND ")
  if(!is.null(s_species_wp_codes)) CA_query = paste0(CA_query, "CA.SPECIES_WP_CODE IN (", s_species_wp_codes, ") AND ")
  if(!is.null(s_catch_unit_codes)) CA_query = paste0(CA_query, "CA.CATCH_UNIT_CODE IN (", s_catch_unit_codes, ") AND ")

  CA_query = paste(CA_query, "1 = 1")

  data = (decorate(query(connection, CA_query), factorize_results, connection = connection))

  return (
    data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
        [!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#' Retrieves *raw* monthly catch-and-effort data from the 'standard' \code{V_LEGACY_CE_STD} or 'official' \code{V_LEGACY_CE_OFF} view on IOTDB, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria
#'
#' The 'standard' version includes the 'FLAG_CODE' column as a combination of the codes from both
#' the flag country and the reporting country for the record, and the 'FLEET_CODE' column as it derives
#' from the combination of FLAG_CODE and fishing gear mapped by the \code{CountryStratVsFleet}
#' (which turns several flag + reporting country into generic fleets such as \code{NEICE}, \code{NEIFR}, \code{NEIPS} and similar).
#' The 'official' version only includes the 'FLEET_CODE' column determined as indicated above.
#'
#' @param use_official A flag that tells whether using the official (if TRUE) or standard (if FALSE) view
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
#' @param connection An ODBC connection to the \code{\link{IOTDB}} database
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_LEGACY_CE} matching the filtering criteria
#' @examples CE_raw(years = seq(1980, 2000, 1), effort_unit_codes = "TRIPS", catch_unit_codes = c("MT"))
#' @examples CE_raw(use_official = TRUE, years = seq(1980, 2000, 1), effort_unit_codes = "TRIPS", catch_unit_codes = c("MT"))
#' @export
CE_raw = function(use_official = TRUE,
                  years = NULL,
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
                  connection = iotc.core.db.connections::getDefaultDBIHandler()(),
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

  CE_query = paste0("SELECT * FROM [meta].V_LEGACY_CE_", ifelse(use_official, "OFF", "STD"), " CE WHERE ")

  if(!is.null(s_years)) CE_query = paste0(CE_query, "CE.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_flag_codes) & use_official) {
    stop("You can't provide flag codes to filter 'official' datasets: use fleet codes instead")
  }

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

  CE_query = paste(CE_query, "1 = 1")

  data = (decorate(query(connection, CE_query), factorize_results, connection = connection))

  return (
    data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
        [!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#' Retrieves *raw* monthly sample data from the 'standard' \code{V_LEGACY_SA_STD} or 'official' \code{V_LEGACY_SA_OFF} view on IOTDB, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria
#'
#' The 'standard' version includes the 'FLAG_CODE' column as a combination of the codes from both
#' the flag country and the reporting country for the record, and the 'FLEET_CODE' column as it derives
#' from the combination of FLAG_CODE and fishing gear mapped by the \code{CountryStratVsFleet}
#' (which turns several flag + reporting country into generic fleets such as \code{NEICE}, \code{NEIFR}, \code{NEIPS} and similar).
#' The 'official' version only includes the 'FLEET_CODE' column determined as indicated above.
#'
#' @param use_official A flag that tells whether using the official (if TRUE) or standard (if FALSE) view
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
#' @param connection An ODBC connection to the \code{\link{IOTDB}} database
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_LEGACY_CA} matching the filtering criteria
#' @examples SF_raw(years = seq(1980, 2000, 1), measure_type_codes = "FL", species_wp_codes = "NERI")
#' @export
SA_raw = function(use_official = TRUE,
                  years = NULL,
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
                  connection = iotc.core.db.connections::getDefaultDBIHandler()(),
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

  SA_query = paste0("SELECT * FROM [meta].V_LEGACY_SA_", ifelse(use_official, "OFF", "STD"), " SA WHERE ")

  if(!is.null(s_years)) SA_query = paste0(SA_query, "SA.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_flag_codes) & use_official) {
    stop("You can't provide flag codes to filter 'official' datasets: use fleet codes instead")
  }

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

  data = (decorate(query(connection, SA_query), factorize_results, connection = connection))

  return (
    data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
        [!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#' Retrieves *raw* monthly size-frequency data from the 'standard' \code{V_LEGACY_SF_STD} or 'official' \code{V_LEGACY_SF_OFF} view on IOTDB, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria
#'
#' The 'standard' version includes the 'FLAG_CODE' column as a combination of the codes from both
#' the flag country and the reporting country for the record, and the 'FLEET_CODE' column as it derives
#' from the combination of FLAG_CODE and fishing gear mapped by the \code{CountryStratVsFleet}
#' (which turns several flag + reporting country into generic fleets such as \code{NEICE}, \code{NEIFR}, \code{NEIPS} and similar).
#' The 'official' version only includes the 'FLEET_CODE' column determined as indicated above.
#'
#' @param use_official A flag that tells whether using the official (if TRUE) or standard (if FALSE) view
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
#' @param connection An ODBC connection to the \code{\link{IOTDB}} database
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_LEGACY_CA} matching the filtering criteria
#' @examples SF_raw(years = seq(1980, 2000, 1), measure_type_codes = "FL", species_wp_codes = "NERI")
#' @export
SF_raw = function(use_official = TRUE,
                  years = NULL,
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
                  connection = iotc.core.db.connections::getDefaultDBIHandler()(),
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

  SF_query = paste0("SELECT * FROM [meta].V_LEGACY_SF_", ifelse(use_official, "OFF", "STD"), " SF WHERE ")

  if(!is.null(s_years)) SF_query = paste0(SF_query, "SF.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_flag_codes) & use_official) {
    stop("You can't provide flag codes to filter 'official' datasets: use fleet codes instead")
  }

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

  data = (decorate(query(connection, SF_query), factorize_results, connection = connection))

  return (
    data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
        [!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#'Alias for \code{\link{NC_raw}}
#'@export
raw.NC = NC_raw

#'Alias for \code{\link{NC_raw}}
#'@export
NC.raw = NC_raw

#'Alias for \code{\link{EF_raw}}
#'@export
raw.EF = EF_raw

#'Alias for \code{\link{EF_raw}}
#'@export
EF.raw = EF_raw

#'Alias for \code{\link{CA_raw}}
#'@export
raw.CA = CA_raw

#'Alias for \code{\link{CA_raw}}
#'@export
CA.raw = CA_raw

#'Alias for \code{\link{CE_raw}}
#'@export
raw.CE = CE_raw

#'Alias for \code{\link{CE_raw}}
#'@export
CE.raw = CE_raw

#'Alias for \code{\link{SA_raw}}
#'@export
raw.SA = SA_raw

#'Alias for \code{\link{SA_raw}}
#'@export
SA.raw = SA_raw

#'Alias for \code{\link{SF_raw}}
#'@export
raw.SF = SF_raw

#'Alias for \code{\link{SF_raw}}
#'@export
SF.raw = SF_raw
