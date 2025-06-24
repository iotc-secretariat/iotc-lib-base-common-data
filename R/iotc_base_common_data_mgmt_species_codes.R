#'Extracts all details of the \code{[meta].SPECIES} table as available on \code{IOTDB}
#'@param connection A connection to the IOTDB
#'@return a data frame with multiple rows (one for each species) and the required columns to model the structure of the table
#'@export
all_species = function(connection = DB_IOTDB()) {
  return(
    cache_get_or_set(CODELISTS_CACHE, cache_key_root("SPECIES"), {
      query(
        connection,
        query = "SELECT * FROM [meta].SPECIES"
      )
    })
  )
}

#'Extracts all distinct details of the \code{[meta].SPECIES} table as available on \code{IOTDB}
#'@param connection A connection to the IOTDB
#'@return a data frame with multiple rows (one for each species) and the required columns to model the structure of the table
#'@export
distinct_species = function(connection = DB_IOTDB()) {
  return(
    cache_get_or_set(CODELISTS_CACHE, cache_key_root("DISTINCT_SPECIES"), {
      query(
        connection,
        query = "
          SELECT DISTINCT
            CODE,
            NAME_EN AS SPECIES,
            NAME_LT AS SPECIES_SCIENTIFIC,
            ASFIS_FAMILY AS SPECIES_FAMILY,
            ASFIS_ORDER AS SPECIES_ORDER,
            SPECIES_CATEGORY_CODE,
            SPECIES_GROUP_CODE,
            WP_CODE AS SPECIES_WP_CODE,
            IS_IOTC AS IS_IOTC_SPECIES,
            IS_AGGREGATE AS IS_SPECIES_AGGREGATE,
            IS_SSI AS IS_SSI,
            IUCN_STATUS AS IUCN_STATUS_CODE,
            IS_BAIT
          FROM
            [meta].SPECIES
        "
      )
    })
  )
}


#'Extracts a species name from the \code{cdeSpecies} table on \code{IOTDB} for a given species code
#'@param species_code A species code
#'@param connection A connection to the IOTDB
#'@return The English name for the provided species code
#'@examples species_name("YFT")
#'@export
species_name = function(species_code, connection = DB_IOTDB()) {
  record = if(!is.null(connection)){
    all_species(connection)[CODE == species_code]
  }else{
    iotc.data.reference.codelists::LEGACY_SPECIES_IOTDB[CODE== species_code]
  }

  if(nrow(record) > 0) return(record$NAME_EN)

  return(NA);
}

main_species_data = function(connection = DB_IOTDB()) {
  S = if(!is.null(connection)){
    all_species(connection)
  }else{
    iotc.data.reference.codelists::LEGACY_SPECIES_IOTDB
  }
  S = merge(S, if(!is.null(connection)) all_wps(connection) else iotc.data.reference.codelists::LEGACY_WORKING_PARTIES_IOTDB, by.x = "WP_CODE", by.y = "CODE")

  names(S)[names(S) == "SORT.x"] = "SORT"
  names(S)[names(S) == "NAME_EN.x"] = "NAME_EN"
  names(S)[names(S) == "SORT.y"]    = "WP_SORT"
  names(S)[names(S) == "NAME_EN.y"] = "WP_NAME_EN"

  S = merge(S, if(!is.null(connection)) all_species_groups(connection) else iotc.data.reference.codelists::LEGACY_SPECIES_GROUPS_IOTDB, by.x = C_SPECIES_GROUP_CODE, by.y = "CODE")
  names(S)[names(S) == "SORT.x"]    = "SORT"
  names(S)[names(S) == "NAME_EN.x"] = "NAME_EN"
  names(S)[names(S) == "SORT.y"]    = "SPECIES_GROUP_SORT"
  names(S)[names(S) == "NAME_EN.y"] = "SPECIES_GROUP_NAME_EN"

  S = merge(S, if(!is.null(connection)) all_species_categories(connection) else iotc.data.reference.codelists::LEGACY_SPECIES_CATEGORIES_IOTDB, by.x = C_SPECIES_CATEGORY_CODE, by.y = "CODE")
  names(S)[names(S) == "SORT.x"]    = "SORT"
  names(S)[names(S) == "NAME_EN.x"] = "NAME_EN"
  names(S)[names(S) == "SORT.y"]    = "SPECIES_CATEGORY_SORT"
  names(S)[names(S) == "NAME_EN.y"] = "SPECIES_CATEGORY_NAME_EN"

  return (S[order(+SPECIES_CATEGORY_SORT, +WP_SORT, +SPECIES_GROUP_SORT, +SORT)])
}

#'Adds various species' metadata to a dataset containing a \code{SPECIES_CODE} column
#'@param data The dataset
#'@param connection A connection to \code{\link{IOTDB}}
#'@return The original dataset augmented with the species metadata if that's the case
#'@export
add_species_metadata = function(data, connection = DB_IOTDB()) {
  if(C_SPECIES_CODE %in% colnames(data)) {
    data = drop_column_if_exists(data, C_SPECIES_WP_CODE)
    data = drop_column_if_exists(data, C_SPECIES_GROUP_CODE)
    data = drop_column_if_exists(data, C_SPECIES_CATEGORY_CODE)
    data = drop_column_if_exists(data, C_IS_IOTC_SPECIES)
    data = drop_column_if_exists(data, C_IS_SPECIES_AGGREGATE)
    data = drop_column_if_exists(data, C_IS_SSI)

    data = drop_column_if_exists(data, C_IUCN_STATUS_CODE)
    data = drop_column_if_exists(data, C_SPECIES)
    data = drop_column_if_exists(data, "SPECIES_SCIENTIFIC")
    data = drop_column_if_exists(data, "SPECIES_FAMILY")
    data = drop_column_if_exists(data, "SPECIES_ORDER")

    all_distinct_species = distinct_species(connection)

    data = merge(x = data, y = all_distinct_species, by.x = C_SPECIES_CODE, by.y = "CODE", all.x = TRUE, allow.cartesian = TRUE)
  }

  return (data)
}
