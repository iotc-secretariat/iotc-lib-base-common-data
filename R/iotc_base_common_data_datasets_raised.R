#' Retrieves *raised* yearly nominal catch data from the \code{V_NC_RAISED} view on \code{WP_CE_raised},
#' optionally filtering the records according to a variable set of user-provided, AND-ed criteria.
#'
#' It only includes the 'FLEET_CODE' column as it derives from the combination of FLAG_CODE (flag country + reporting country and
#' fishing gear mapped by the \code{CountryStratVsFleet} (which turns several flag + reporting country into generic fleets such as \code{NEICE}, \code{NEIFR}, \code{NEIPS} and similar).
#'
#' Also, the table is only limited to the five main IOTC species, which are here fully disaggregated, and therefore the column \code{IS_IOTC_SPECIES} is fixed to TRUE
#' and the column \code{IS_SPECIES_AGGREGATE} is fixed to FALSE
#'
#' @param years A vector of years the extraction should be limited to
#' @param fleet_codes A vector of flag codes the extraction should be limited to
#' @param gear_codes A vector of gear codes the extraction should be limited to#'
#' @param fishery_codes A vector of fishery codes the extraction should be limited to
#' @param fishery_group_codes A vector of fishery group codes the extraction should be limited to
#' @param species_codes A vector of species codes the extraction should be limited to
#' @param species_category_codes A vector of species category codes (BILLFISH, SEERFISH, NERITIC, TEMPERATE, TROPICAL, TUNAS_NEI, SHARKS, MANTAS, SEABIRDS, CETACEANS, TURTLES, OTHERS) the extraction should be limited to
#' @param species_group_codes A vector of species group codes (BILLFISH, SEERFISH, TUNAS, SHARKS, OTHERS) the extraction should be limited to
#' @param species_wp_codes A vector of species WP codes (BILL, NERI, TEMP, TROP, BYCT, UNCL) the extraction should be limited to
#' @param connection An ODBC connection to the \code{\link{WP_CE_RAISED}} database
#' @param connection_IOTDB An ODBC connection to the \code{\link{IOTDB}} database
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_NC_RAISED} matching the filtering criteria
#' @examples NC_raised(years = seq(1980, 2000, 1), species_codes = c("ALB", "BET"))
#' @export
NC_raised = function(years = NULL,
                     fleet_codes = NULL,
                     gear_codes = NULL,
                     fishery_codes = NULL,
                     fishery_group_codes = NULL,
                     species_codes = NULL,
                     species_category_codes = NULL,
                     species_group_codes = NULL,
                     species_wp_codes = NULL,
                     connection = DB_WP_CE_RAISED(),
                     connection_IOTDB = iotc.core.db.connections::getDefaultDBIHandler()(),
                     factorize_results = TRUE) {

  s_years         = join_values(years)
  s_fleet_codes   = join_strings(fleet_codes)
  s_gear_codes    = join_strings(gear_codes)
  s_species_codes = join_strings(species_codes)
  s_species_category_codes = join_strings(species_category_codes)
  s_species_group_codes    = join_strings(species_group_codes)
  s_species_wp_codes       = join_strings(species_wp_codes)

  NC_query = "SELECT * FROM V_NC_RAISED NC WHERE "

  if(!is.null(s_years)) NC_query = paste0(NC_query, "NC.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_fleet_codes)) NC_query = paste0(NC_query, "NC.FLEET_CODE IN (", s_fleet_codes, ") AND ")
  if(!is.null(s_gear_codes))  NC_query = paste0(NC_query,  "NC.GEAR_CODE IN (", s_gear_codes, ") AND ")

  if(!is.null(s_species_codes)) NC_query = paste0(NC_query, "NC.SPECIES_CODE IN (", s_species_codes, ") AND ")
  if(!is.null(s_species_category_codes)) NC_query = paste0(NC_query, "NC.SPECIES_CATEGORY_CODE IN (", s_species_category_codes, ") AND ")
  if(!is.null(s_species_group_codes)) NC_query = paste0(NC_query, "NC.SPECIES_GROUP_CODE IN (", s_species_group_codes, ") AND ")
  if(!is.null(s_species_wp_codes)) NC_query = paste0(NC_query, "NC.SPECIES_WP_CODE IN (", s_species_wp_codes, ") AND ")

  NC_query = paste(NC_query, "1 = 1")

  data = (decorate(query(connection, NC_query), factorize_results, connection = connection_IOTDB))

  #Filters by fishery code / fishery group code, if required (the two data fields are not available within the query)
  data = data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
  data = data[!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]

  return(data)
}

#' Retrieves *raised* monthly geo-referenced catch data from the \code{V_CA_RAISED} view on \code{WP_CE_raised},
#' optionally filtering the records according to a variable set of user-provided, AND-ed criteria.
#'
#' It only includes the 'FLEET_CODE' column as it derives from the combination of FLAG_CODE (flag country + reporting country and
#' fishing gear mapped by the \code{CountryStratVsFleet} (which turns several flag + reporting country into generic fleets such as \code{NEICE}, \code{NEIFR}, \code{NEIPS} and similar).
#'
#' Also, the table is only limited to the five main IOTC species, which are here fully disaggregated, and therefore the column \code{IS_IOTC_SPECIES} is fixed to TRUE
#' and the column \code{IS_SPECIES_AGGREGATE} is fixed to FALSE
#'
#' @param years A vector of years the extraction should be limited to
#' @param fleet_codes A vector of flag codes the extraction should be limited to
#' @param gear_codes A vector of gear codes the extraction should be limited to#'
#' @param fishery_codes A vector of fishery codes the extraction should be limited to
#' @param fishery_group_codes A vector of fishery group codes the extraction should be limited to
#' @param species_codes A vector of species codes the extraction should be limited to
#' @param species_category_codes A vector of species category codes (BILLFISH, SEERFISH, NERITIC, TEMPERATE, TROPICAL, TUNAS_NEI, SHARKS, MANTAS, SEABIRDS, CETACEANS, TURTLES, OTHERS) the extraction should be limited to
#' @param species_group_codes A vector of species group codes (BILLFISH, SEERFISH, TUNAS, SHARKS, OTHERS) the extraction should be limited to
#' @param species_wp_codes A vector of species WP codes (BILL, NERI, TEMP, TROP, BYCT, UNCL) the extraction should be limited to
#' @param connection An ODBC connection to the \code{\link{WP_CE_RAISED}} database
#' @param connection_IOTDB An ODBC connection to the \code{\link{IOTDB}} database
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{V_CA_RAISED} matching the filtering criteria
#' @examples CA_raised(years = seq(1980, 2000, 1), species_codes = c("ALB", "BET"))
#' @export
CA_raised = function(years = NULL,
                     fleet_codes = NULL,
                     gear_codes = NULL,
                     fishery_codes = NULL,
                     fishery_group_codes = NULL,
                     species_codes = NULL,
                     species_category_codes = NULL,
                     species_group_codes = NULL,
                     species_wp_codes = NULL,
                     connection = DB_WP_CE_RAISED(),
                     connection_IOTDB = iotc.core.db.connections::getDefaultDBIHandler()(),
                     factorize_results = TRUE) {

  s_years         = join_values(years)
  s_fleet_codes   = join_strings(fleet_codes)
  s_gear_codes    = join_strings(gear_codes)
  s_species_codes = join_strings(species_codes)
  s_species_category_codes = join_strings(species_category_codes)
  s_species_group_codes    = join_strings(species_group_codes)
  s_species_wp_codes       = join_strings(species_wp_codes)

  CA_query = "SELECT * FROM V_CA_RAISED CA WHERE "

  if(!is.null(s_years)) CA_query = paste0(CA_query, "CA.YEAR IN (", s_years, ") AND ")

  if(!is.null(s_fleet_codes)) CA_query = paste0(CA_query, "CA.FLEET_CODE IN (", s_fleet_codes, ") AND ")
  if(!is.null(s_gear_codes)) CA_query = paste0(CA_query, "CA.GEAR_CODE IN (", s_gear_codes, ") AND ")

  if(!is.null(s_species_codes)) CA_query = paste0(CA_query, "CA.SPECIES_CODE IN (", s_species_codes, ") AND ")
  if(!is.null(s_species_category_codes)) CA_query = paste0(CA_query, "CA.SPECIES_CATEGORY_CODE IN (", s_species_category_codes, ") AND ")
  if(!is.null(s_species_group_codes)) CA_query = paste0(CA_query, "CA.SPECIES_GROUP_CODE IN (", s_species_group_codes, ") AND ")
  if(!is.null(s_species_wp_codes)) CA_query = paste0(CA_query, "CA.SPECIES_WP_CODE IN (", s_species_wp_codes, ") AND ")

  CA_query = paste(CA_query, "1 = 1")

  data = (decorate(query(connection, CA_query), factorize_results, connection = connection_IOTDB))

  return (
    data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
        [!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#'Alias for \code{\link{NC_raised}}
#'@export
raised.NC = NC_raised

#'Alias for \code{\link{NC_raised}}
#'@export
NC.raised = NC_raised

#'Alias for \code{\link{CA_raised}}
#'@export
raised.CA = CA_raised

#'Alias for \code{\link{CA_raised}}
#'@export
CA.raised = CA_raised
