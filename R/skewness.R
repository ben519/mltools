#' Skewness
#'
#' @export
#' @import data.table

skewness <- function(dt){
  # Display the skewness of each field
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  Count <- NULL
  
  #--------------------------------------------------

  skewnessList <- list()

  for(colname in colnames(dt)){
    skewnessList[[colname]] <- dt[, list(Count=.N, Pcnt=.N/nrow(dt)), by=colname][order(Count, decreasing=TRUE)]
  }

  return(skewnessList)
}
