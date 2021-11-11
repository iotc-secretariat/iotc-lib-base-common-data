#' Retrieves monthly effort data from the \code{V_ROS_EF} view on \code{\link{ROS_ANALYSIS}}, optionally filtering the records
#' according to a variable set of user-provided, AND-ed criteria
#'
#' @param years A vector of years the extraction should be limited to
#' @param date_from A starting date (as '\code{yyyy-MM-dd}') the extraction should be limited to
#' @param date_to An ending date (\code{yyyy-MM-dd}') the extraction should be limited to
#' @param fleet_codes A vector of flag codes the extraction should be limited to
#' @param vessel_names A vector of vessel names the extraction should be limited to
#' @param buoy_brands A vector of buoy brands the extraction should be limited to
#' @param buoy_models A vector of buoy models the extraction should be limited to
#' @param buoy_IDs A vector of buoy IDs the extraction should be limited to
#' @param longitude_range A vector of min-max longitudes (as decimal degrees) the extraction should be limited to
#' @param latitude_range A vector of min-max latitudes (as decimal degrees) the extraction should be limited to
#' @param IO_area_code An IOTC area code the extraction should be limited to. It can be a CWP grid code or an irregular grid code. The intersection is approximated using the 1x1 grid grids overlaping with the given area
#' @param intersection_level When an IOTC area code is set, limits the results to those points that are in grids whose intersection with the area amounts to the provided level (in the 0 to 1 range)
#' @param CWP_1_grids A vector of CWP 1x1 grid codes the the extraction should be limited to.
#' @param CWP_5_grids A vector of CWP 5x5 grid codes the the extraction should be limited to.
#' @param connection An ODBC connection to the \code{\link{BUOY_DATA}} database
#' @return a data frame with the records from \code{BUOY_DATA} matching the filtering criteria
#' @examples BU_DATA(2020, fleet_codes = "EUESP", longitude_range = c(40, 50), latitude_range = c(-15, -25))
#' @examples BU_DATA(2020, fleet_codes = "EUESP", IO_area_code = "IRSYCEZ")
#' @export
BU_data = function(years = NULL,
                   date_from = NULL,
                   date_to = NULL,
                   fleet_codes = NULL,
                   vessel_names = NULL,
                   buoy_brands = NULL,
                   buoy_models = NULL,
                   buoy_IDs = NULL,
                   longitude_range = NULL,
                   latitude_range = NULL,
                   IO_area_code = NULL,
                   intersection_level = NULL,
                   CWP_1_grids = NULL,
                   CWP_5_grids = NULL,
                   connection = DB_BUOYS(),
                   IOTCStatistics_connection = DB_IOTCSTATISTICS()) {
  s_years         = join_values(years)
  s_fleet_codes   = join_strings(fleet_codes)
  s_vessel_names  = join_strings(vessel_names)
  s_buoy_brands   = join_strings(buoy_brands)
  s_buoy_models   = join_strings(buoy_models)
  s_buoy_IDs      = join_strings(buoy_IDs)
  s_CWP_1_grids   = join_strings(CWP_1_grids)
  s_CWP_5_grids   = join_strings(CWP_5_grids)

  BU_query = paste0("SELECT * FROM [dbo].BUOY_DATA BU WHERE ")

  if(!is.null(s_years)) BU_query = paste0(BU_query,         "BU.YEAR IN (", s_years, ") AND ")

  if(!is.null(date_from)) BU_query = paste0(BU_query,       "BU.POSITION_DATE >= '", date_from, "' AND ")
  if(!is.null(date_to))   BU_query = paste0(BU_query,       "BU.POSITION_DATE <= '", date_to, "' AND ")

  if(!is.null(s_fleet_codes)) BU_query = paste0(BU_query,   "BU.FLEET_CODE IN (", s_fleet_codes, ") AND ")

  if(!is.null(s_vessel_names)) BU_query = paste0(BU_query,  "BU.VESSEL_NAME IN (", s_vessel_names, ") AND ")

  if(!is.null(s_buoy_brands)) BU_query = paste0(BU_query,   "BU.BUOY_BRAND IN (", s_buoy_brands, ") AND ")
  if(!is.null(s_buoy_models)) BU_query = paste0(BU_query,   "BU.BUOY_MODEL IN (", s_buoy_models, ") AND ")
  if(!is.null(s_buoy_IDs))    BU_query = paste0(BU_query,   "BU.BUOY_ID    IN (", s_buoy_IDs, ") AND ")

  if(!is.null(longitude_range)) BU_query = paste0(BU_query, "BU.POSITION_LON BETWEEN ", longitude_range[1], " AND ", longitude_range[2], " AND ")
  if(!is.null(latitude_range))  BU_query = paste0(BU_query, "BU.POSITION_LAT BETWEEN ", latitude_range[1], " AND ", latitude_range[2], " AND ")

  if(!is.null(s_CWP_1_grids)) BU_query = paste0(BU_query,   "BU.CWP_1_GRID_CODE IN (", s_CWP_1_grids, ") AND ")
  if(!is.null(s_CWP_5_grids)) BU_query = paste0(BU_query,   "BU.CWP_5_GRID_CODE IN (", s_CWP_1_grids, ") AND ")

  if(!is.null(IO_area_code)) {
    grid_1x1_codes =
      grid_intersections_by_source_grid_type(
        source_grid_type_code = grid_1x1,
        target_grid_codes = IO_area_code,
        IOTCStatistics_connection
      )

    if(!is.null(intersection_level))
      grid_1x1_codes = grid_1x1_codes[PROPORTION >= intersection_level]

    if(is_available(grid_1x1_codes))
      BU_query = paste0(BU_query, "BU.CWP_1_GRID_CODE IN (", join_strings(grid_1x1_codes$SOURCE_FISHING_GROUND_CODE), ") AND")
  }

  BU_query = paste(BU_query, "1 = 1")

  data = query(connection, BU_query)

  data$VESSEL_NAME = factor(data$VESSEL_NAME)
  data$BUOY_BRAND  = factor(data$BUOY_BRAND)
  data$BUOY_MODEL  = factor(data$BUOY_MODEL)
  data$BUOY_ID     = factor(data$BUOY_ID)
  #data$POSITION_DATE = factor(data$POSITION_DATE)
  data$CWP_1_GRID_CODE = factor(data$CWP_1_GRID_CODE)
  data$CWP_5_GRID_CODE = factor(data$CWP_5_GRID_CODE)

  return(data)
}

#'Foo
#'@export
add_buoy_events = function(data) {
  TIMELINE = data[, .(START_DATE = min(POSITION_DATE), END_DATE = max(POSITION_DATE)), by = .(BUOY_ID)]

  data$EVENT = "TRANSMITTING"

  data = merge(x = data, y = TIMELINE, by = "BUOY_ID", all.x = TRUE)

  data[POSITION_DATE == START_DATE]$EVENT = "START_TRANSMISSION"
  data[POSITION_DATE == END_DATE]$EVENT   = "STOP_TRANSMISSION"

  data$START_DATE = NULL
  data$END_DATE = NULL

  return(data)
}
