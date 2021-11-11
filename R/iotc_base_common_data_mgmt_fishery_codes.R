#'Adds standard fishery group codes to provided data frame according to the \code{FISHERY_GROUP} column
#'@export
add_fishery_group_codes = function(data) {
  if(!(FISHERY_GROUP_CODE %in% colnames(data))) data[, FISHERY_GROUP_CODE := "OT"]
  if(FISHERY_GROUP %in% colnames(data)) {
    data[FISHERY_GROUP == "Purse seine", FISHERY_GROUP_CODE := "PS"]
    data[FISHERY_GROUP == "Longline",    FISHERY_GROUP_CODE := "LL"]
    data[FISHERY_GROUP == "Gillnet",     FISHERY_GROUP_CODE := "GN"]
    data[FISHERY_GROUP == "Baitboat",    FISHERY_GROUP_CODE := "BB"]
    data[FISHERY_GROUP == "Line",        FISHERY_GROUP_CODE := "LI"]
    data[FISHERY_GROUP == "Handline",    FISHERY_GROUP_CODE := "LI"]
    data[FISHERY_GROUP == "Trolling",    FISHERY_GROUP_CODE := "LI"]
  }

  return (data)
}

#'Marks species-specific minor fisheries groups as 'OT'
#'@export
update_fishery_group_codes_for_species = function(data) {
  data = add_fishery_group_codes(data)

  data[SPECIES_CODE == "BET" & !FISHERY_GROUP_CODE %in% c("PS", "LL", "LI"), FISHERY_GROUP_CODE := "OT"]
  data[SPECIES_CODE == "SKJ" & !FISHERY_GROUP_CODE %in% c("PS", "GN", "BB"), FISHERY_GROUP_CODE := "OT"]
  data[SPECIES_CODE == "YFT" & !FISHERY_GROUP_CODE %in% c("PS", "LL", "LI", "GN", "BB"), FISHERY_GROUP_CODE := "OT"]
  data[SPECIES_CATEGORY_CODE == "NERITIC" & !FISHERY_GROUP_CODE %in% c("PS", "LI", "GN"), FISHERY_GROUP_CODE := "OT"]
  data[SPECIES_CATEGORY_CODE == "SEERFISH" & !FISHERY_GROUP_CODE %in% c("PS", "LI", "GN"), FISHERY_GROUP_CODE := "OT"]
  data[SPECIES_CATEGORY_CODE == "TEMPERATE" & !FISHERY_GROUP_CODE %in% c("PS", "LL", "GN"), FISHERY_GROUP_CODE := "OT"]
  data[SPECIES_CATEGORY_CODE == "BILLFISH" & !FISHERY_GROUP_CODE %in% c("LL", "LI", "GN"), FISHERY_GROUP_CODE := "OT"]
  #data[SPECIES_CODE == "SWO" & !FISHERY_GROUP_CODE %in% c( "LL", "LI", "GN"), FISHERY_GROUP_CODE := "OT"]

  return (factorize_all(data))
}

#'Marks species group-specific minor fisheries groups as 'OT'
#'@export
update_fishery_group_codes_for_species_group = function(data, species_group = WP_TROP, connection = DB_IOTDB()) {
  data = add_fishery_codes(data)

  data = (
    data[SPECIES_WP_CODE == WP_TROP && !FISHERY_GROUP_CODE %in% c("PS", "LL", "LI", "GN", "BB"), FISHERY_GROUP_CODE := "OT"]
        [SPECIES_WP_CODE == WP_NERI && !FISHERY_GROUP_CODE %in% c("PS",       "LI", "GN"      ), FISHERY_GROUP_CODE := "OT"]
        [SPECIES_WP_CODE == WP_TEMP && !FISHERY_GROUP_CODE %in% c("PS", "LL",       "GN"      ), FISHERY_GROUP_CODE := "OT"]
        [SPECIES_WP_CODE == WP_BILL && !FISHERY_GROUP_CODE %in% c(      "LL", "LI", "GN"      ), FISHERY_GROUP_CODE := "OT"]
  )

  return (decorate(data, connection))
}

#'Adds standard fishery codes to provided data frame according to the \code{FISHERY_GROUP} and \code{CATCH_SCHOOL_TYPE_CODE} columns
#'@export
add_fishery_codes = function(data) {
  has_school_type = SCHOOL_TYPE_CODE %in% colnames(data)
  has_catch_school_type = CATCH_SCHOOL_TYPE_CODE %in% colnames(data)

  if(has_school_type) {
    data[FISHERY_GROUP_CODE == "PS" & SCHOOL_TYPE_CODE %in% c("FD", "FS"), FISHERY_CODE := "PSFS"]
    data[FISHERY_GROUP_CODE == "PS" & SCHOOL_TYPE_CODE %in% c("LA", "LS"), FISHERY_CODE := "PSLS"]
    data[FISHERY_GROUP_CODE == "PS" & SCHOOL_TYPE_CODE == "UNCL", FISHERY_CODE := "PSOT"]
  } else if(has_catch_school_type) {
    data[FISHERY_GROUP_CODE == "PS" & CATCH_SCHOOL_TYPE_CODE %in% c("FD", "FS"), FISHERY_CODE := "PSFS"]
    data[FISHERY_GROUP_CODE == "PS" & CATCH_SCHOOL_TYPE_CODE %in% c("LA", "LS"), FISHERY_CODE := "PSLS"]
    data[FISHERY_GROUP_CODE == "PS" & CATCH_SCHOOL_TYPE_CODE == "UNCL", FISHERY_CODE := "PSOT"]
  } else {
    data[FISHERY_GROUP_CODE == "PS" , FISHERY_CODE := "PSOT"]
  }

  data = (
    data[FISHERY_GROUP_CODE == "LL" & !GEAR_CODE %in% c("LL", "LLEX", "LLOB", "FLL"), FISHERY_CODE := "LLO"]
        [FISHERY_GROUP_CODE == "LL" &  GEAR_CODE %in% c("LL", "LLEX", "LLOB"), FISHERY_CODE := "LLD"]
        [FISHERY_GROUP_CODE == "LL" &  GEAR_CODE == "FLL", FISHERY_CODE := "LLF"]
        [FISHERY_GROUP_CODE == "GN", FISHERY_CODE := "GN"]
        [FISHERY_GROUP_CODE == "BB", FISHERY_CODE := "BB"]
        [FISHERY_GROUP_CODE == "HL" & GEAR_CODE != "LLCO", FISHERY_CODE := "LIH"]
        [FISHERY_GROUP_CODE == "HL" & GEAR_CODE == "LLCO", FISHERY_CODE := "LIC"]
        [FISHERY_GROUP_CODE == "TL", FISHERY_CODE := "LIT"]
        [!FISHERY_CODE %in% c("PSOT", "PSFS", "PSLS", "LLO", "LLD", "LLF", "GN", "BB", "LIC", "LIH", "LIT"), FISHERY_CODE := "OT"]
  )

  return (data)
}

add_fishery_codes_old = function(data) {
  has_school_type = SCHOOL_TYPE_CODE %in% colnames(data)
  has_catch_school_type = CATCH_SCHOOL_TYPE_CODE %in% colnames(data)

  if(has_school_type) {
    data[FISHERY_GROUP_CODE == "PS" & SCHOOL_TYPE_CODE %in% c("FD", "FS"), FISHERY_CODE := "PSFS"]
    data[FISHERY_GROUP_CODE == "PS" & SCHOOL_TYPE_CODE %in% c("LA", "LS"), FISHERY_CODE := "PSLS"]
    data[FISHERY_GROUP_CODE == "PS" & SCHOOL_TYPE_CODE == "UNCL", FISHERY_CODE := "PSOT"]
  } else if(has_catch_school_type) {
    data[FISHERY_GROUP_CODE == "PS" & CATCH_SCHOOL_TYPE_CODE %in% c("FD", "FS"), FISHERY_CODE := "PSFS"]
    data[FISHERY_GROUP_CODE == "PS" & CATCH_SCHOOL_TYPE_CODE %in% c("LA", "LS"), FISHERY_CODE := "PSLS"]
    data[FISHERY_GROUP_CODE == "PS" & CATCH_SCHOOL_TYPE_CODE == "UNCL", FISHERY_CODE := "PSOT"]
  } else {
    data[FISHERY_GROUP_CODE == "PS", FISHERY_CODE := "PSOT"]
  }

  data = (
    data[FISHERY_GROUP_CODE == "LL" & !GEAR_CODE %in% c("LL", "LLEX", "LLOB", "FLL"), FISHERY_CODE := "LLO"]
        [FISHERY_GROUP_CODE == "LL" &  GEAR_CODE %in% c("LL", "LLEX", "LLOB"), FISHERY_CODE := "LLD"]
        [FISHERY_GROUP_CODE == "LL" &  GEAR_CODE == "FLL", FISHERY_CODE := "LLF"]
        [FISHERY_GROUP_CODE == "GN", FISHERY_CODE := "GN"]
        [FISHERY_GROUP_CODE == "BB", FISHERY_CODE := "BB"]
        [FISHERY_GROUP_CODE == "HL" & GEAR_CODE != "LLCO", FISHERY_CODE := "LIH"]
        [FISHERY_GROUP_CODE == "HL" & GEAR_CODE == "LLCO", FISHERY_CODE := "LIC"]
        [FISHERY_GROUP_CODE == "TL", FISHERY_CODE := "LIT"]
        [!FISHERY_CODE %in% c("PSOT", "PSFS", "PSLS", "LLO", "LLD", "LLF", "GN", "BB", "LIC", "LIH", "LIT"), FISHERY_CODE := "OT"]
  )

  return (data)
}

#'Adds fishery codes for a specific species to provided data frame according to the \code{FISHERY_GROUP} column
#'@export
update_fishery_codes_for_species = function(data) {
  data = add_fishery_codes(data)

  data = (
    data[SPECIES_CODE == "BET" & !FISHERY_CODE %in% c("PSOT", "PSFS", "PSLS", "LLO", "LLD", "LLF", "LIC", "LIH", "LIT"), FISHERY_CODE := "OT"]
        [SPECIES_CODE == "SKJ" & !FISHERY_CODE %in% c("PSOT", "PSFS", "PSLS", "GN", "BB"), FISHERY_CODE := "OT"]
        [SPECIES_CODE == "YFT" & !FISHERY_CODE %in% c("PSOT", "PSFS", "PSLS", "LLO", "LLD", "LLF", "GN", "BB", "LIC", "LIH", "LIT"), FISHERY_CODE := "OT"]
        [SPECIES_GROUP_CODE == "NERI" & !FISHERY_CODE %in% c("PSOT", "PSFS", "PSLS", "GN", "LIC", "LIH", "LIT"), FISHERY_CODE := "OT"]
        [SPECIES_GROUP_CODE == "TEMP" & !FISHERY_CODE %in% c("PSOT", "PSFS", "PSLS", "GN", "LLD", "LLF"), FISHERY_CODE := "OT"]
        [SPECIES_GROUP_CODE == "BILL" & !FISHERY_CODE %in% c("LLD", "LLF", "LLO", "LIC", "LIH", "LIT", "GN"), FISHERY_CODE := "OT"]
        [SPECIES_CODE == "SWO" & !FISHERY_CODE %in% c("LLD", "LLF", "LLO", "LIC", "GN"), FISHERY_CODE := "OT"]

    #To be updated with all other IOTC species (from the BILL, NERI and TEMP species groups)
  )

  return (factorize_all(data))
}

#'Adds fishery codes for a specific species to provided data frame according to the \code{FISHERY_GROUP} column
#'@export
update_fishery_codes_for_species_group = function(data, connection = DB_IOTDB()) {
  data = add_fishery_codes(data)

  data = (
    data[SPECIES_WP_CODE == WP_TROP & !FISHERY_CODE %in% c("PSOT", "PSFS", "PSLS", "LLO", "LLD", "LLF", "GN", "BB", "LIC", "LIH", "LIT"), FISHERY_CODE := "OT"]
        [SPECIES_WP_CODE == WP_NERI & !FISHERY_CODE %in% c("PSOT", "PSFS", "PSLS", "LIC", "LIH", "LIT", "GN"), FISHERY_CODE := "OT"]
        [SPECIES_WP_CODE == WP_TEMP & !FISHERY_CODE %in% c("PSOT", "PSFS", "PSLS", "LLD", "LLF", "GN"), FISHERY_CODE := "OT"]
        [SPECIES_WP_CODE == WP_BILL & !FISHERY_CODE %in% c("LLD", "LLF", "LLO", "LIC", "LIH", "LIT", "GN"), FISHERY_CODE := "OT"]
  )

  return (decorate(data, connection))
}
