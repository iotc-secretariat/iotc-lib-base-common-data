#' Retrieves *estimated* yearly nominal catch data from the \code{V_LEGACY_NC_EST} view on IOTDB (based on est_NCDissag),
#' optionally filtering the records according to a variable set of user-provided, AND-ed criteria.
#'
#' It only includes the 'FLEET_CODE' column as it derives from the combination of FLAG_CODE (flag country + reporting country and
#' fishing gear mapped by the \code{CountryStratVsFleet} (which turns several flag + reporting country into generic fleets such as \code{NEICE}, \code{NEIFR}, \code{NEIPS} and similar).
#' The 'official' version only includes the 'FLEET_CODE' column determined as indicated above.
#'
#' Also, the table is only limited to IOTC species, which are here fully disaggregated, and therefore the column \code{IS_IOTC_SPECIES} is fixed to TRUE
#' and the column \code{IS_SPECIES_AGGREGATE} is fixed to FALSE
#'
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
#' @return a data frame with the records from \code{V_LEGACY_NC_EST} matching the filtering criteria
#' @examples NC_est(years = seq(1980, 2000, 1), species_codes = c("ALB", "SBF"))
#' @export
NC_est = function(years = NULL,
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

  s_years          = join_values(years)
  s_fleet_codes    = join_strings(fleet_codes)
  s_gear_codes     = join_strings(gear_codes)
  s_species_codes  = join_strings(species_codes)
  s_species_category_codes = join_strings(species_category_codes)
  s_species_group_codes = join_strings(species_group_codes)
  s_species_wp_codes    = join_strings(species_wp_codes)

  NC_query = "SELECT * FROM [meta].V_LEGACY_NC_EST NC WHERE "

  if(!is.null(s_years)) NC_query = paste0(NC_query, "NC.YEAR IN (", s_years, ") AND ")
  if(!is.null(s_fleet_codes)) NC_query = paste0(NC_query, "NC.FLEET_CODE IN (", s_fleet_codes, ") AND ")

  if(!is.null(s_gear_codes)) NC_query = paste0(NC_query, "NC.GEAR_CODE IN (", s_gear_codes, ") AND ")

  if(!is.null(s_species_codes)) NC_query = paste0(NC_query, "NC.SPECIES_CODE IN (", s_species_codes, ") AND ")
  if(!is.null(s_species_category_codes)) NC_query = paste0(NC_query, "NC.SPECIES_CATEGORY_CODE IN (", s_species_category_codes, ") AND ")
  if(!is.null(s_species_group_codes)) NC_query = paste0(NC_query, "NC.SPECIES_GROUP_CODE IN (", s_species_group_codes, ") AND ")
  if(!is.null(s_species_wp_codes)) NC_query = paste0(NC_query, "NC.SPECIES_WP_CODE IN (", s_species_wp_codes, ") AND ")

  NC_query = paste(NC_query, "1 = 1")

  data = (decorate(query(connection, NC_query), factorize_results, connection = connection))

  #Filters by fishery code / fishery group code, if required (the two data fields are not available within the query)
  data = data[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
  data = data[!is_available(fishery_group_codes) | FISHERY_GROUP_CODE %in% fishery_group_codes, ]

  if(factorize_results) return(factorize_fishing_grounds(data))
  else return(data)
}

#' Retrieves *estimated* size-frequency data from the \code{vwSF<species_code>} view on IOTDB, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria
#'
#' It only includes the 'FLEET_CODE' column as it derives from the combination of FLAG_CODE (flag country + reporting country and
#' fishing gear mapped by the \code{CountryStratVsFleet} (which turns several flag + reporting country into generic fleets such as \code{NEICE}, \code{NEIFR}, \code{NEIPS} and similar).
#' The 'official' version only includes the 'FLEET_CODE' column determined as indicated above.
#'
#' @param species_code A species code the extraction should be limited to.
#' @param years A vector of years the extraction should be limited to
#' @param fleet_codes A vector of flag codes the extraction should be limited to
#' @param gear_codes A vector of gear codes the extraction should be limited to#'
#' @param fishery_codes A vector of fishery codes the extraction should be limited to
#' @param fishery_groups A vector of fishery groups the extraction should be limited to
#' It can be one among: \code{ALB}, \code{BET}, \code{SKJ}, \code{YFT}, \code{BLM}, \code{BUM}, \code{MLS}, \code{SFA}, \code{SWO},
#' \code{BLT}, \code{COM}, \code{FRI}, \code{GUT}, \code{KAW}, \code{LOT} or \code{SKH}
#' @param connection An ODBC connection to the \code{\link{IOTDB}} database
#' @param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#' @return a data frame with the records from \code{vwSF<species_code>} matching the filtering criteria
#' @examples SF_est(years = seq(1980, 2000, 1), species_code = "ALB")
#' @export
SF_est = function(species_code,
                  years = NULL,
                  fleet_codes = NULL,
                  gear_codes = NULL,
                  fishery_codes = NULL,
                  fishery_group_codes = NULL,
                  school_type_codes = NULL,
                  connection = iotc.core.db.connections::getDefaultDBIHandler()(),
                  factorize_results = TRUE) {

  species_filter = species_code

  if(species_code %in% c("ALB", "BET", "SKJ", "YFT", "BUM", "BLM", "MLS", "SFA", "SWO")) species_filter = species_code
  else if(species_code %in% c("BLT", "COM", "FRI", "GUT", "KAW", "LOT")) species_filter = "NER"
  else if(species_code %in% c("ALV", "BSH", "BTH", "FAL", "OCS", "POR", "PSK", "SMA", "SPL")) species_filter = "SKH"
  else if(species_code %in% c("SKH", "NER")) { species_filter = species_code; species_code = NA; }

  table = paste0("vwSF", species_filter)
  columns = ""

  for(c in seq(1, 149, 1))
    columns = paste0(columns, "C", sprintf("%03d", c), ", ")

  columns = paste(columns, "C150")

  s_years          = join_values(years)
  s_fleet_codes    = join_strings(fleet_codes)
  s_gear_codes     = join_strings(gear_codes)

  s_school_type_codes = join_strings(school_type_codes)

  length_bins = c()

  for(c in 1:150)
    length_bins = c(length_bins, sprintf("C%03d", c))

  SF_query = paste0("
    WITH FLEET_FISHERY_TYPE AS (
  	  SELECT DISTINCT
    		FLEET,
    		FISHERY_CODE,
    		FISHERY_TYPE_CODE
    	FROM
    		[meta].TEMP_CSVF
    )
    SELECT
      SF.Year AS YEAR,
      SF.MonthStart AS MONTH_START,
      SF.MonthEnd AS MONTH_END,
      SF.Fleet AS FLEET_CODE,
      SF.Gear AS GEAR_CODE,
      G.EngDescr AS GEAR,
      CASE
        WHEN G.LAggCESF IN ('Purse Seine', 'Purse seine') THEN 'PS'
        WHEN G.LAggCESF = 'Longline' THEN 'LL'
        WHEN G.LAggCESF = 'Gillnet'  THEN 'GN'
        WHEN G.LAggCESF = 'Baitboat' THEN 'BB'
        WHEN G.LAggCESF = 'Handline' THEN 'HL'
        WHEN G.LAggCESF = 'Trolling' THEN 'TL'
        ELSE 'OT'
      END AS FISHERY_GROUP_CODE,
    	CSVF.FISHERY_TYPE_CODE,
      SF.SchooLType AS SCHOOL_TYPE_CODE,
      SF.Grid AS FISHING_GROUND_CODE,
      SP.WP_CODE AS SPECIES_WP_CODE,
      SP.SPECIES_GROUP_CODE,
      SP.SPECIES_CATEGORY_CODE,
      SP.CODE AS SPECIES_CODE,
      SP.IS_IOTC AS IS_IOTC_SPECIES,
      SP.IS_AGGREGATE AS IS_SPECIES_AGGREGATE,
      SP.IS_SSI,
      SF.MeasureType AS MEASURE_TYPE_CODE,
      'CM' AS MEASURE_UNIT_CODE,
      SF.FirstClassLow AS FIRST_CLASS_LOW,
      SF.SizeInterval AS SIZE_INTERVAL,
      SF.TnoFish AS TOT_NUM_FISH,
      SF.TkgFish AS TOT_KG_FISH,
  ",
      join_values(length_bins),
  "
    FROM")

    SF_query = paste(SF_query, table, "SF")

    SF_query = paste(SF_query, "
    INNER JOIN
      cdeGears G
    ON
      SF.Gear = G.ACode
    INNER JOIN
      FLEET_FISHERY_TYPE CSVF
    ON
      CSVF.FISHERY_CODE = SF.Gear AND
      CSVF.FLEET = SF.Fleet
    INNER JOIN
      [meta].SPECIES SP
    ON
      SF.Species = SP.IOTDB_code
    WHERE ")

  if(!is.na(species_code)) SF_query = paste0(SF_query, "SP.CODE = '", species_code, "' AND ")

  if(!is.null(s_years)) SF_query = paste0(SF_query, "SF.Year IN (", s_years, ") AND ")
  if(!is.null(s_fleet_codes)) SF_query = paste0(SF_query, "SF.Fleet IN (", s_fleet_codes, ") AND ")
  if(!is.null(s_gear_codes)) SF_query = paste0(SF_query, "SF.Gear IN (", s_gear_codes, ") AND ")

  if(!is.null(s_school_type_codes)) SF_query = paste0(SF_query, "SF.SchoolType IN (", s_school_type_codes, ") AND ")

  SF_query = paste0(SF_query, "1 = 1;")

  data = query(connection, SF_query)

  measure_vars = c()

  for(c in 1:150) {
    measure_vars = c(measure_vars, sprintf("C%03d", c))
  }

  SF = melt.data.table(data, measure.vars = measure_vars)[value > 0]
  SF[, CLASS_LOW  := FIRST_CLASS_LOW + SIZE_INTERVAL * ( as.numeric( gsub( pattern = 'C', replacement = '', x = variable ) ) - 1 )]
  SF[, CLASS_HIGH := CLASS_LOW + SIZE_INTERVAL ]

  #Rename melted value column to FISH_COUNT
  colnames(SF)[which(colnames(SF) == "value")] = "FISH_COUNT"

  #Remove unneeded columns
  SF$FIRST_CLASS_LOW = NULL
  SF$SIZE_INTERVAL   = NULL
  SF$TOT_NUM_FISH    = NULL
  SF$TOT_KG_FISH     = NULL
  SF$variable        = NULL

  #Add missing column
  SF$SEX_CODE   = "UNCL"
  SF$RAISE_CODE = "UNCL"

  SF = decorate(SF[is.na(species_code) | SPECIES_CODE == species_code, ], factorize_results, connection = connection)

  return (
    SF[!is_available(fishery_codes) | FISHERY_CODE %in% fishery_codes, ]
      [!is_available(fishery_group_codes)| FISHERY_GROUP_CODE %in% fishery_group_codes, ]
  )
}

#'Alias for \code{\link{NC_est}}
#'@export
est.NC = NC_est

#'Alias for \code{\link{NC_est}}
#'@export
NC.est = NC_est

#'Alias for \code{\link{SF_est}}
#'@export
est.SF = SF_est

#'Alias for \code{\link{SF_est}}
#'@export
SF.est = SF_est
