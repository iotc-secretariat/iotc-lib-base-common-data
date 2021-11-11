EQUATIONS_CACHE = new.env(hash = TRUE)

#' Resets the content of the codelists cache, triggering a reload from the database
#' the first time a codelist is accessed
#' @export
reset_equations_cache = function() {
  # Never seen anything so convoluted in my entire life...
  # Once again, R S-U-C-K-S!!!
  rm(list=ls(envir = EQUATIONS_CACHE), envir = EQUATIONS_CACHE)
}

#'Converts a size measurement to another for a given species (if a corresponding equation is available)
#'@param species_code The species code
#'@param source_measure_code The code identifying the type of source size measurement
#'@param target_measure_code The code identifying the type of target size measurement
#'@param source_measure The source measurement to convert
#'@param fishery_group_code A fishery group code, in case specific convertion equations for the species exist
#'@return the source measurement converted to the target size measurement
#'@export
#'@examples convert_measure("BET", "FL", "KG", 100) #Converts a fork length of 100 cm to rounded weight for a BET
#'@examples convert_measure("BET", "FL", "KG", 100, "PS") #Converts a fork length of 100 cm to rounded weight for a BET according to PS-specific equations
convert_measure = function(species_code,
                           source_measure_code,
                           target_measure_code,
                           source_measure,
                           fishery_group_code = NA,
                           connection = DB_IOTDB()) {
  if(!is_available(species_code))        stop("Please provide a species code")
  if(!is_available(source_measure_code)) stop("Please provide a source measure code")
  if(!is_available(target_measure_code)) stop("Please provide a target measure code")
  if(!is_available(source_measure))      stop("Please provide a source measure value to convert")

  key = paste0(species_code, fishery_group_code, source_measure_code, target_measure_code, collapse = "|")

  LW_query = paste0("SELECT * FROM [meta].LW_EQUATIONS LW WHERE ")

  LW_query = paste0(LW_query, "SPECIES_CODE = '", species_code, "' AND ")
  LW_query = paste0(LW_query, "SOURCE_UNIT_CODE = '", source_measure_code, "' AND ")
  LW_query = paste0(LW_query, "TARGET_UNIT_CODE = '", target_measure_code, "' AND ")

  if(is_available(fishery_group_code)) LW_query = paste0(LW_query, "FISHERY_GROUP_CODE = '", fishery_group_code, "' AND ")
  else LW_query = paste0(LW_query, "FISHERY_GROUP_CODE IS NULL AND ")

  LW_query = paste0(LW_query, "1 = 1")

  equation =
    cache_get_or_set(
      cache = EQUATIONS_CACHE,
      key = key,
      { query(connection, LW_query) }
    )

  if(!is_available(equation) | nrow(equation) == 0) stop("Unable to identify any conversion equation based on the provided parameters")

  if(nrow(equation) > 1) stop(paste0(nrow(equation), " distinct conversion equations identified by the provided parameters"))

  A = equation$A_COEFFICIENT
  B = equation$B_COEFFICIENT

  R = equation$RAISING_FACTOR
  M = source_measure

  return(
    eval(
      parse(
        text = equation$FUNCTION_DESCRIPTION
      )
    )
  )
}
