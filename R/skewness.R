#' Skewness
#'
#' @export
#' @import data.table

skewness <- function(dt){
  # Display the skewness of each field

  skewnessList <- list()

  for(colname in colnames(dt)){
    skewnessList[[colname]] <- dt[, list(Count=.N, Pcnt=.N/nrow(dt)), by=colname][order(Count, decreasing=TRUE)]
  }

  return(skewnessList)
}
