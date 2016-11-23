#' @title
#' Skewness
#'
#' @description
#' Calculates the skewness of each field in a data.table
#'
#' @details
#' Counts the frequency of each value in each column, then displays the results in descending order
#' 
#' @param dt A data.table
#' 
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#' skewness(alien.train)

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
