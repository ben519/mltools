#' @title
#' Root Mean Square Error
#'
#' @description
#' Calculate Root-Mean-Square Error (Deviation)
#' 
#' For the ith sample, Squared Error is calculated as SE = (prediction - actual)^2. 
#' RMSE is then sqrt(mean(squared errors)).
#'
#' @details
#' Calculate Root-Mean-Square Error (Deviation)
#' 
#' @param preds A vector of prediction values in [0, 1]
#' @param actuals A vector of actuals values in {0, 1}, or {FALSE, TRUE}
#' @param weights Optional vectors of weights
#' @param na.rm Should (prediction, actual) pairs with at least one NA value be ignored?
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

rmse <- function(preds = NULL, actuals = NULL, weights = 1, na.rm = FALSE){
  # root-mean-square error
  
  if(is.logical(weights))
    stop("weights given as logical but should be numeric")
  
  if(!is.logical(na.rm))
    stop("na.rm should be logical")
  
  if(length(weights) > 1 & length(weights) != length(preds))
    stop("weights should be the same length as preds")
  
  result <- sqrt(mltools::mse(preds=preds, actuals=actuals, weights=weights, na.rm=na.rm))
  
  return(result)
}