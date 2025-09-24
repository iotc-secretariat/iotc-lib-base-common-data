#'Produces a data-quality report (by dataset) for a given species / species group among the IOTC ones, optionally
#'filtered by years, fleet and fishery
#'
#'@param year_from The starting year
#'@param year_from The ending year
#'@param fleet_code A fleet code
#'@param gear_code A gear code
#'@param species_code A species code
#'@param species_group_code A species group code (among \code{BILL}, \code{NERI}, \code{TEMP}, \code{TROP})
#'@param factorize_results Whether the result data table has to be factorized. Among other things, when set to \code{TRUE} maps all less-important species to \code{UNCL} (within their species group)
#'@return a data table with the quality reports for the strata selected through the filtering criteria above
#'@examples data_quality(year_from = 1969, year_to = 2019, species_group_code = "TROP")
#'@export
data_quality = function(year_from = NULL,
                        year_to = NULL,
                        fleet_code = NULL,
                        gear_code = NULL,
                        species_code = NULL,
                        species_group_code = NULL,
                        connection = iotc.core.db.connections::getDefaultDBIHandler()(),
                        factorize_result = TRUE) {
  params = c()

  if(!is.null(year_from)) params = c(params, paste0("@fromYear = ", year_from))
  if(!is.null(year_to)) params = c(params, paste0("@toYear = ", year_to))
  if(!is.null(fleet_code)) params = c(params, paste0("@fleet = '", fleet_code, "'"))
  if(!is.null(gear_code)) params = c(params, paste0("@gear = '", gear_code, "'"))
  if(!is.null(species_code)) params = c(params, paste0("@species = '", species_code, "'"))
  if(!is.null(species_group_code)) params = c(params, paste0("@speciesGroup = '", species_group_code, "'"))

  invocation_query = paste("EXEC [meta].[PROC_DATA_QUALITY]", paste(params, collapse=","))

  data = query(connection, invocation_query)

  data = as.data.table(data)

  data[, FISHERY_GROUP_CODE := ifelse(FISHERY_GROUP == "Longline", "LL",
                                      ifelse(FISHERY_GROUP == "Purse seine", "PS",
                                             ifelse(FISHERY_GROUP == "Handline", "HL",
                                                    ifelse(FISHERY_GROUP == "Gillnet", "GN",
                                                           ifelse(FISHERY_GROUP == "Trolling", "TL",
                                                                  ifelse(FISHERY_GROUP == "Baitboat", "BB",
                                                                         "OT"))))))]

  data[, ":=" (CATCH   = as.numeric(CATCH),
               CATCH_CE= as.numeric(CATCH_CE),
               SAMPLES = as.numeric(SAMPLES),
               #YEAR               = as.factor(YEAR),
               FLEET_CODE         = as.factor(FLEET_CODE),
               GEAR_CODE          = as.factor(GEAR_CODE),
               FISHERY_GROUP      = as.factor(FISHERY_GROUP),
               FISHERY_TYPE_CODE  = as.factor(FISHERY_TYPE_CODE),
               SPECIES_CODE       = as.factor(SPECIES_CODE),
               SPECIES_GROUP_CODE = as.factor(SPECIES_GROUP_CODE),
               NC = as.factor(NC),
               CE = as.factor(CE),
               SF = as.factor(SF))]

  return (decorate(data, factorize_result, connection = connection))

  #return (
  #  result[, .(YEAR,
  #             FLEET_CODE,
  #             FLEET,
  #             GEAR_CODE,
  #             GEAR,
  #             FISHERY_CODE,
  #             FISHERY,
  #             FISHERY_GROUP_CODE,
  #             FISHERY_GROUP,
  #             FISHERY_TYPE_CODE,
  #             FISHERY_TYPE,
  #             SPECIES_CODE,
  #             SPECIES,
  #             SPECIES_CATEGORY_CODE,
  #             SPECIES_CATEGORY,
  #             SPECIES_GROUP_CODE,
  #             SPECIES_GROUP,
  #             SPECIES_WP_CODE,
  #             SPECIES_WP,
  #             IS_IOTC_SPECIES,
  #             IS_SPECIES_AGGREGATE,
  #             IS_SSI,
  #             IUCN_STATUS_CODE,
  #             IUCN_STATUS,
  #             CATCH,
  #             CATCH_CE,
  #             SAMPLES,
  #             NC,
  #             CE,
  #            SF)
  #       ]
  #   )
}

#'Same as \code{\link{data_quality}}
#'@export
data.quality = data_quality
