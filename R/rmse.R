#' @title
#' Root Mean Square Error
#'
#' @description
#' Calculate Root-Mean-Square Error (Deviation)
#'
#' @details
#' Calculate Root-Mean-Square Error (Deviation)
#' 
#' @param preds A vector of prediction values in [0, 1]
#' @param actuals A vector of actuals values in {0, 1}, or {FALSE, TRUE}
#' @param na.rm Should (prediction, actual) pairs with at least one NA value be ignored?
#' @param weights Optional vectors of weights
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

rmse <- function(preds=NULL, actuals=NULL, na.rm=FALSE, weights=1){
  # root-mean-square error
  
  if(length(weights) > 1 & length(weights) != length(preds))
    stop("weights should be the same length as preds")
  
  result <- sqrt(mltools::mse(preds=preds, actuals=actuals, na.rm=na.rm, weights=weights))
  
  return(result)
}