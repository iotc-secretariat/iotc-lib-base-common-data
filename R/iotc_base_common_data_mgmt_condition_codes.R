#'Adds condition type codes to provided data frame according to the \code{CONDITION_CODE} column
#'@export
add_condition_type_codes = function(data) {
  if((C_CONDITION_CODE %in% colnames(data))) {
    if(!C_CONDITION_TYPE_CODE %in% colnames(data)) {
      data$CONDITION_TYPE_CODE = character()
    }

    data[CONDITION_CODE %in% c("A0", "A1", "A2", "A3", "AU")]$CONDITION_TYPE_CODE = "ALIVE"
    data[CONDITION_CODE %in% c("S")]$CONDITION_TYPE_CODE = "STUNNED"
    data[CONDITION_CODE %in% c("U")]$CONDITION_TYPE_CODE = "UNKNOWN"
    data[CONDITION_CODE %in% c("D")]$CONDITION_TYPE_CODE = "DEAD"
  }

  return (data)
}
