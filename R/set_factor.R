#' @title
#' Set Factor
#'
#' @description
#' Convience method for dealing with factors. Map a list of vectors to a list of factor vectors (1-1 mapping) such that
#' the factor vectors all have the same levels - the unique values of the union of all the vectors in the list. Optionally
#' group all low frequency values into a "_other_" level.
#' 
#' @param vectorList A list of values to convert to factors
#' @param aggregationThreshold Values which appear this many times or less will be grouped into the level "_other_"
#'
#' @export
#' @import data.table
#'
#' @examples
#' x <- c("a", "b", "c", "c")
#' y <- c("a", "d", "d")
#' set_factor(list(x, y))
#' set_factor(list(x, y), aggregationThreshold=1)

set_factor <- function(vectorList, aggregationThreshold=0){
  # Converts each vector in vectorList to an unordered factor with levels equal to the values of their union
  # Any values whose count is equal to or less than aggregationThreshold will be set to "_other_"
  # NA will remain NA
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  Count <- NULL
  Value <- NULL
  
  #--------------------------------------------------

  valsDT <- data.table(Value=unlist(lapply(vectorList, as.character)), ListIdx=rep(1:length(vectorList), times=sapply(vectorList, length)))

  # Reduce cardinality by setting elements with count <= aggregationThreshold to "_other_"
  if(aggregationThreshold > 0){
    valsDT[, Count := .N, by=Value]
    valsDT[Count <= aggregationThreshold & !is.na(Value), Value := "_other_"]
  }

  # Build the output vectors
  newlevels <- sort(unique(valsDT$Value))
  valsDT[, Value := factor(Value, levels=newlevels)]

  # Return list of results
  result <- split(valsDT$Value, f=valsDT$ListIdx)
  names(result) <- names(vectorList)
  return(result)
}
