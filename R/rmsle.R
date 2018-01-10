#' @title
#' Root Mean Square Logarithmic Error
#'
#' @description
#' Calculate Root-Mean-Square-Logarithmic Error (Deviation)
#' 
#' For the ith sample, Squared Logarithmic Error is calculated as SLE = (log(prediction + 1) - log(actual + 1))^2. 
#' RMSLE is then sqrt(mean(squared logarithmic errors)). Note the '+1' in the calculation of SLE which avoids taking the logarithm of 0
#' for data which may include 0s.
#'
#' @details
#' Calculate Root-Mean-Square-Logarithmic Error (Deviation)
#' 
#' @param preds A vector of prediction values in [0, 1]
#' @param actuals A vector of actuals values in {0, 1}, or {FALSE, TRUE}
#' @param weights Optional vectors of weights
#' @param na.rm Should (prediction, actual) pairs with at least one NA value be ignored?
#'
#' @examples
#' preds <- c(1.0, 2.0, 9.5)
#' actuals <- c(0.9, 2.1, 10.0)
#' rmsle(preds, actuals)
#' 
#' @export
#' @import data.table

rmsle <- function(preds = NULL, actuals = NULL, weights = 1, na.rm = FALSE){
  # root-mean-square-logarithmic error
  
  if(is.logical(weights))
    stop("weights given as logical but should be numeric")
  
  if(!is.logical(na.rm))
    stop("na.rm should be logical")
  
  if(length(weights) > 1 & length(weights) != length(preds))
    stop("weights should be the same length as preds")
  
  result <- sqrt(mltools::msle(preds=preds, actuals=actuals, weights=weights, na.rm=na.rm))
  
  return(result)
}
