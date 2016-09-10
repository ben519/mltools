#' Set factor variable
#'
#' @export
#' @import data.table

set_factor <- function(vectorList, aggregationThreshold=0){
  # Converts each vector in vectorList to an unordered factor with levels equal to the values of their union
  # Any values whose count is equal to or less than aggregationThreshold will be set to "_other_"
  # NA will remain NA

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

# vec1 <- c("a", "b", "d", NA)
# vec2 <- c("b", "c")
# setFactor(list(factor(vec1), vec2), 0)
