#' Same as \code{NC_raw(use_official = FALSE, ...)}
#' @export
NC_raw_std = function(years = NULL,
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
                      factorize_result = TRUE) {
  return ((NC_raw(FALSE, years, flag_codes, fleet_codes, gear_codes, fishery_codes, fishery_group_codes, species_codes, species_category_codes, species_group_codes, species_wp_codes, connection, factorize_result)))
}

#' Same as \code{NC_raw(use_official = TRUE, ...)}
#' @export
NC_raw_off = function(years = NULL,
                      fleet_codes = NULL,
                      gear_codes = NULL,
                      fishery_codes = NULL,
                      fishery_group_codes = NULL,
                      species_codes = NULL,
                      species_category_codes = NULL,
                      species_group_codes = NULL,
                      species_wp_codes = NULL,
                      connection = iotc.core.db.connections::getDefaultDBIHandler()(),
                      factorize_result = TRUE) {
  return ((NC_raw(TRUE, years, NULL, fleet_codes, gear_codes, fishery_codes, fishery_group_codes, species_codes, species_category_codes, species_group_codes, species_wp_codes, connection, factorize_result)))
}

#' Same as \code{EF_raw(use_official = TRUE, ...)}
#' @export
EF_raw_off = function(years = NULL,
                      fleet_codes = NULL,
                      gear_codes = NULL,
                      fishery_codes = NULL,
                      fishery_group_codes = NULL,
                      school_type_codes = NULL,
                      effort_unit_codes = NULL,
                      connection = iotc.core.db.connections::getDefaultDBIHandler()(),
                      factorize_result = TRUE) {
  return ((EF_raw(TRUE, years, NULL, fleet_codes, gear_codes, fishery_codes, fishery_group_codes, school_type_codes, effort_unit_codes, connection, factorize_result)))
}

#' Same as \code{EF_std(use_official = FALSE, ...)}
#' @export
EF_raw_std = function(years = NULL,
                      flag_codes = NULL,
                      fleet_codes = NULL,
                      gear_codes = NULL,
                      fishery_codes = NULL,
                      fishery_group_codes = NULL,
                      school_type_codes = NULL,
                      effort_unit_codes = NULL,
                      connection = iotc.core.db.connections::getDefaultDBIHandler()(),
                      factorize_result = TRUE) {
  return ((EF_raw(FALSE, years, flag_codes, fleet_codes, gear_codes, fishery_codes, fishery_group_codes, school_type_codes, effort_unit_codes, connection, factorize_result)))
}

#' Same as \code{CA_raw(use_official = TRUE, ...)}
#' @export
CA_raw_off = function(years = NULL,
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
                      factorize_result = TRUE) {

  return (CA_raw(TRUE, years, NULL, fleet_codes, gear_codes, fishery_codes, fishery_group_codes, school_type_codes, species_codes, species_category_codes, species_group_codes, species_wp_codes, catch_unit_codes, connection, factorize_result))
}

#' Same as \code{CA_raw(use_official = FALSE, ...)}
#' @export
CA_raw_std = function(years = NULL,
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
                      factorize_result = TRUE) {

  return (CA_raw(FALSE, years, flag_codes, fleet_codes, gear_codes, fishery_codes, fishery_group_codes, school_type_codes, species_codes, species_category_codes, species_group_codes, species_wp_codes, catch_unit_codes, connection, factorize_result))
}

#' Same as \code{CE_raw(use_official = TRUE, ...)}
#' @export
CE_raw_off = function(years = NULL,
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
                      factorize_result = TRUE) {
  return (CE_raw(TRUE, years, NULL, fleet_codes, gear_codes, fishery_codes, fishery_group_codes, school_type_codes, species_codes, species_category_codes, species_group_codes, species_wp_codes, effort_unit_codes, catch_unit_codes, connection, factorize_result))
}

#' Same as \code{CE_raw(use_official = FALSE, ...)}
#' @export
CE_raw_std = function(years = NULL,
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
                      factorize_result = TRUE) {
  return (CE_raw(FALSE, years, flag_codes, fleet_codes, gear_codes, fishery_codes, fishery_group_codes, school_type_codes, species_codes, species_category_codes, species_group_codes, species_wp_codes, effort_unit_codes, catch_unit_codes, connection, factorize_result))
}

#' Same as \code{SA_raw(use_official = TRUE, ...)}
#' @export
SA_raw_off = function(years = NULL,
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
                      factorize_result = TRUE) {
  return (SA_raw(TRUE, years, NULL, fleet_codes, gear_codes, fishery_codes, fishery_group_codes, school_type_codes, species_codes, species_category_codes, species_group_codes, species_wp_codes, measure_type_codes, connection, factorize_result))
}

#' Same as \code{SA_raw(use_official = FALSE, ...)}
#' @export
SA_raw_std = function(years = NULL,
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
                      factorize_result = TRUE) {
  return (SA_raw(FALSE, years, flag_codes, fleet_codes, gear_codes, fishery_codes, fishery_group_codes, school_type_codes, species_codes, species_category_codes, species_group_codes, species_wp_codes, measure_type_codes, connection, factorize_result))
}

#' Same as \code{SF_raw(use_official = TRUE, ...)}
#' @export
SF_raw_off = function(years = NULL,
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
                      factorize_result = TRUE) {
  return (SF_raw(TRUE, years, NULL, fleet_codes, gear_codes, fishery_codes, fishery_group_codes, school_type_codes, species_codes, species_category_codes, species_group_codes, species_wp_codes, measure_type_codes, sex_codes, connection, factorize_result))
}

#' Same as \code{SF_raw(use_official = FALSE, ...)}
#' @export
SF_raw_std = function(years = NULL,
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
                      factorize_result = TRUE) {
  return (SF_raw(FALSE, years, flag_codes, fleet_codes, gear_codes, fishery_codes, fishery_group_codes, school_type_codes, species_codes, species_category_codes, species_group_codes, species_wp_codes, measure_type_codes, sex_codes, connection, factorize_result))
}
