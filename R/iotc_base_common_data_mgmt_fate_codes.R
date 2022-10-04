#'Adds fate type codes to provided data frame according to the \code{FATE_CODE} column
#'@export
add_fate_type_codes = function(data) {
  if((C_FATE_CODE %in% colnames(data))) {
    if(!C_FATE_TYPE_CODE %in% colnames(data)) {
      data$FATE_TYPE_CODE = character()
    }

    data[FATE_CODE %in% c("RCC", "RFR", "RFT", "RFL")]$FATE_TYPE_CODE = "RETAINED"
    data[FATE_CODE %in% c("ESC")]$FATE_TYPE_CODE = "ESCAPED"
    data[FATE_CODE %in% c("DPQ", "DFR", "DRB", "DUD", "DTS", "DUS", "DUN")]$FATE_TYPE_CODE = "DISCARDED"
  }

  return (data)
}
