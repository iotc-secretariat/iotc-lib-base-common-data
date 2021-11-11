#'DEPRECATED
#'Adds the \code{FLEET} (fleet english name), \code{FLEET_SORT} (fleet sorting order), \code{CPC_CODE} (fleet CPC code) and \code{STATUS} (fleet CPC status) to a data table already containing a \code{FLEET_CODE} column
#'@export
add_fleet_names_ = function(data, connection = DB_IOTDB()) {
  all_fleet_codes = all_fleets(connection)

  updated = merge(data, all_fleet_codes, by.x = FLEET_CODE, by.y = "CODE", all.x = TRUE)
  names(updated)[names(updated) == "NAME_EN"] = FLEET
  names(updated)[names(updated) == "SORT"]    = "FLEET_SORT"
  names(updated)[names(updated) == "STATUS"]  = "FLEET_STATUS"

  return (updated)
}

#'Extracts all fleet codes and names from the \code{[meta].FLEETS} table on \code{\link{IOTDB}}
#'@param connection A connection to the IOTDB
#'@return a data frame with multiple rows (one for each fleet) and the following columns:
#'\code{SORT} (the fleet sorting order)
#'\code{CODE} (the fleet code, e.g., AUS, EUFRA)
#'\code{CPC_CODE} (the CPC code for the fleet, e.g., AUS, EU)
#'\code{FLEET_NAME_EN} (the fleet english name)
#'\code{STATUS} (the status of the fleet CPC: one among \code{CPC}, \code{NON_CPC}, \code{NEI})
#'@export
all_fleets = function(connection = DB_IOTDB()) {
  if(!exists("FLEETS", CODELISTS_CACHE)) {
    data =
      query(
        connection,
        query = "SELECT * FROM [meta].FLEETS")

    assign("FLEETS", data, envir = CODELISTS_CACHE)
  }

  return (get("FLEETS", envir = CODELISTS_CACHE))
}

#'DEPRECATED
#'Extracts a fleet name from the \code{CountryStratVsFleet} table on \code{IOTDB} for a given fleet code
#'@param fleet_code A fleet code
#'@param connection A connection to the IOTDB
#'@return The English name for the provided fleet code
#'@examples fleet_name("EU_FRA")
#'@export
fleet_name_ = function(fleet_code, connection = DB_IOTDB()) {
  record = all_fleets(connection)["CODE" == fleet_code]

  if(nrow(record) > 0) return(record$NAME_EN)

  return(NA);
}

#'DEPRECATED
sanitize_ = function(string_value) {
  string_value = gsub("(^Eu\\.)([a-z])(.+)", "EU,\\U\\2\\L\\3", string_value, perl = TRUE)
  string_value = gsub("(^Uk\\.)([a-z])(.+)", "UK,\\U\\2\\L\\3", string_value, perl = TRUE)
  string_value = gsub("(^Nei\\.)([a-z])(.+)", "NEI,\\U\\2\\L\\3", string_value, perl = TRUE)
  string_value = gsub("(Eez\\_)(.+)", "EEZ of \\2", string_value, perl = TRUE)
  string_value = gsub("(.+)(\\.)([a-z])(.+)", "\\1 - \\U\\3\\L\\4", string_value, perl = TRUE)

  return (string_value)
}
