#' @title
#' Root Mean Square Error
#'
#' @description
#' Calculate Root-mean-square error (deviation)
#'
#' @details
#' Calculate Root-mean-square error (deviation)
#' 
#' @param preds A vector of prediction values in {1, 0}, or {TRUE, FALSE}
#' @param actuals A vector of actuals values in {1, 0}, or {TRUE, FALSE}
#' @param na.rm Should NA values be ignored?
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Root-mean-square_deviation}
#'
#' @examples
#' preds <- c(1.0, 2.0, 9.5)
#' actuals <- c(0.9, 2.1, 10.0)
#' rmse(preds, actuals)
#' 
#' @export
#' @import data.table

rmse <- function(preds=NULL, actuals=NULL, na.rm=FALSE){
  # Root-mean-square error
  
  result <- sqrt(mean((preds - actuals)^2, na.rm=na.rm))
  
  return(result)
}