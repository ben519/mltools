#' @title
#' Relative Position
#' 
#' @description
#' Scale a vector of values to the range [0, 1] based on rank/position
#' 
#' @details
#' Values are ranked and then scaled to the range [0, 1]. Ties result in the same relative position 
#' (e.g. \code{relative_position(c(1, 2, 2, 3))} returns the vector \code{c(0.0 0.5 0.5 1.0))}. NAs remain as NAs.
#' 
#' @param vals vector of values
#'
#' @examples
#' relative_position(1:10)
#' relative_position(c(1, 2, 2, 3))
#' relative_position(c(1, NA, 3, 4))
#'
#' @export
#' @import data.table

relative_position <- function(vals){
  # Returns the ranked position of each value, scaled to [0, 1]

  if(length(vals) == 1){
    if(is.na(vals)) return(NA) else return(0.5)
  } else{
    return((frank(vals, na.last="keep") - 1) / (sum(!is.na(vals)) - 1))
  }
}
