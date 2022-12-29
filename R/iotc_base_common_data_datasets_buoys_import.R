list_all_files = function(folder) {
  return(
    list.files(
      path = paste(folder, sep = "/"),
      pattern = c("xls", "xlsx"),
      recursive = TRUE,
      full.names = TRUE
    )
  )
}

read_file = function(file_name) {
  return(
    read.xlsx(
      xlsxFile = file_name,
      sheet = "Data",
      startRow = 1,
      skipEmptyCols = TRUE,
      colNames = FALSE
    )
  )
}

convert_file = function(xls) {
  form = data.table(read_file(xls))

  FLEET_CODE  = sanitize_fleet(toupper(as.character(form[4, 5])))
  VESSEL_NAME = sanitize_vessel_name(toupper((as.character(form[10, 4]))))
  VESSEL_IOTC_NUMBER = sanitize_vessel_IOTC_number(toupper((as.character(form[10, 6]))))

  YEAR    = as.integer(form[5, 5])
  MONTH   = as.integer(substr(form[6, 5], 1, 2))

  form = form[-(1:21)]

  data =
    form[ !is.na(X5) &              # Has a buoy id...
          !is.na(as.numeric(X6)) &  # ...and a longitude...
          !is.na(as.numeric(X7)),   # ...and a latitude
          .(
            FLEET_CODE,
            VESSEL_NAME,
            VESSEL_IOTC_NUMBER,
            BUOY_ID = X5,
            BUOY_BRAND = extract_buoy_brand(X5),
            BUOY_MODEL = extract_buoy_model(X5),
            POSITION_DATE  = as.Date(paste(YEAR, MONTH, as.integer(X4), sep = "-"), format = "%Y-%m-%d"),
            POSITION_YEAR  = YEAR,
            POSITION_MONTH = MONTH,
            POSITION_DAY   = as.integer(X4),
            POSITION_LON   = as.numeric(X7),
            POSITION_LAT   = as.numeric(X6),
            CWP_1_GRID_CODE = Vectorize(convert_to_CWP_grid)(lon = as.numeric(X7), lat = as.numeric(X6), grid_1x1),
            CWP_5_GRID_CODE = Vectorize(convert_to_CWP_grid)(lon = as.numeric(X7), lat = as.numeric(X6), grid_5x5)
          )
    ][!is.na(POSITION_DATE)] # Has a date

  strata = data[, .(VESSEL_IOTC_NUMBER, BUOY_ID, POSITION_YEAR, POSITION_MONTH, POSITION_DAY)]
  strata = strata[, .N, by = names(strata)]

  dups = strata[N > 1]

  if(nrow(dups) > 0) {
    print(paste(nrow(dups), "duplicate rows in file", xls))

    print(dups)

    data =
      unique( # This returns only one row per each combination of the provided columns (in 'by')
        data,
        by=c("VESSEL_IOTC_NUMBER", "BUOY_ID", "POSITION_YEAR", "POSITION_MONTH", "POSITION_DAY")
      )
  }

  return(data)
}

delete_existing_data = function(vessel_IOTC_number, year, month, connection = DB_BUOYS()) {
  delete =
    paste0("DELETE FROM [dbo].BUOY_DATA
            WHERE
            VESSEL_IOTC_NUMBER = '", vessel_IOTC_number, "' AND
            POSITION_YEAR  = ", year, " AND
            POSITION_MONTH = ", month)

  query(connection, delete)
}

sanitize_fleet = function(flag) {
  if(flag == "EU - SPAIN" |
     flag == "ESP - EC-Spain" | 
     flag == "ESP-EC-SPAIN")     return("EUESP")
  if(flag == "EU - FRANCE" |
     flag == "FRA - EC-FRANCE")  return("EUFRA")
  if(flag == "EU - ITALY" |
     flag == "ITA - EC-ITALY")   return("EUITA")
  if(flag == "MUS - MAURITIUS" |
     flag == "MUS - MAURICE")    return("MUS")
  if(flag == "SYC - SEYCHELLES" |
     flag == "SEYCHELLES")       return("SYC")
  if(flag == "JPN - JAPAN")      return("JPN")
  if(flag == "TZA - TANZANIA (UNITED REPUBLIC OF)") return("TZA")
  if(flag == "OMN - OMAN") return("OMN")

  warning(paste("Unknown flag", flag))

  return(flag)
}

sanitize_vessel_name = function(vessel_name) {
  if(vessel_name == "BELLE-ISLE")     return("BELLE ISLE")
  if(vessel_name == "MORN SELSELWA")  return("MORN SESELWA")

  return(str_replace_all(vessel_name, "[^a-zA-Z]", " "))
}

sanitize_vessel_IOTC_number = function(vessel_IOTC_number) {
  if(vessel_IOTC_number == "IOTC14063") return("IOTC014063")

  return(vessel_IOTC_number)
}

extract_buoy_model = function(buoy_id) {
  str_extract(buoy_id, "ISD\\+|ISL\\+|M3\\+|MGO|M3I|M4I|SLX\\+|DSL\\+|T8E|T8X|Te7|T7\\+|Orbit")
}

extract_buoy_brand = function(buoy_id) {
  model = extract_buoy_model(buoy_id)

  return(
    fifelse(model %in% c("M3+", "M3I", "M4I", "MGO"), "MI",
      fifelse(model %in% c("T8E", "T8X", "Te7", "T7+"), "ZUNIBAL",
        fifelse(model %in% c("Orbit"), "THALOS",
          "SATLINK"
        )
      )
    )
  )
}

#'Processes all buoy data in a given folder, storing the result in the \code{\link{BUOY_DATA}} database
#'@param folder_path The path of the folder containing the source Excel file (standard IOTC format)
#'@export
process_buoy_data_in_folder = function(folder_path) {
  files = list_all_files(folder_path)

  print(paste("Processing folder", folder_path, "containing", length(files), "files..."))

  if(length(files) == 0) {
    print(paste("Folder", folder_path, "does not exist or contains no file"))

    return(NA)
  }

  all_data = NA

  for(f in 1:length(files)) {
    if(!is_available(all_data)) all_data = process_buoy_data_in_file(f, files[f])
    else all_data = rbind(all_data, process_buoy_data_in_file(f, files[f]))
  }

  if(has_duplicates(all_data))
    all_data = remove_duplicates(all_data)

  return(all_data)
}

#'Processes all buoy data in a file  folder, storing the result in the \code{\link{BUOY_DATA}} database
#'@param index An optional index (ordinal value) for the file being processed. Makes sense only when invoked by the \code{\link{process_buoy_data_in_folder}} function
#'and is used for logging reasons only
#'@param file_path The path of the source Excel file (standard IOTC format)
#'@export
process_buoy_data_in_file = function(index = NA, file_path) {
  data = convert_file(file_path)

  print(paste0("Processing file", ifelse(is.na(index), " ", paste0(" #", index)), " '", file_path, "' (", nrow(data), " rows)"))

  if(has_duplicates(data))
    data = remove_duplicates(data)

  return(data)
}

#'Persists a dataset in the target database (generally, \code{\link{BUOY_DATA}})
#'@param delete_existing To remove records for the same strata from the target table (\code{BUOY_DATA}) before importing the data
#'@param connection A connection to the \code{\link{BUOY_DATA}} database
#'@export
persist_buoy_data = function(data, delete_existing = FALSE, connection = DB_BUOYS()) {
  if(is_available(data)) {
    if(delete_existing) {
      keys = unique(data, by=c("VESSEL_IOTC_NUMBER", "POSITION_YEAR", "POSITION_MONTH"))
      keys = keys[!is.na(VESSEL_IOTC_NUMBER)]

      for(r in 1:nrow(keys)) {
        delete_existing_data(keys[r]$VESSEL_IOTC_NUMBER, keys[r]$POSITION_YEAR, keys[r]$POSITION_MONTH)
      }
    }

    dbAppendTable(connection, "BUOY_DATA", data)
  } else {
    warning("No data to persist")
  }
}

has_duplicates = function(data) {
  strata = data  [, .(VESSEL_IOTC_NUMBER, BUOY_ID, POSITION_YEAR, POSITION_MONTH, POSITION_DAY)]
  strata = strata[, .N, by = names(strata)]

  dups = nrow(strata[N > 1])

  if(dups > 0) {
    print(paste(dups, "duplicate rows found in data"))
  }

  return(dups > 0)
}

remove_duplicates = function(data) {
  return(
    unique( # This returns only one row per each combination of the provided columns (in 'by')
      data,
      fromLast = TRUE, # Forces the removal of "old" duplicates rather than "new" ones
      by=c("VESSEL_IOTC_NUMBER", "BUOY_ID", "POSITION_YEAR", "POSITION_MONTH", "POSITION_DAY")
    )
  )
}
