#' Get relative positions
#'
#' @export
#' @import data.table

relative_position <- function(vals){
  # Returns the ranked position of each value, scaled to (0, 1)

  if(length(vals) == 1){
    if(is.na(vals)) return(NA) else return(0.5)
  } else{
    return((frank(vals, na.last="keep") - 1) / (sum(!is.na(vals)) - 1))
  }
}
