#' @title
#' Mean Square Logarithmic Error
#'
#' @description
#' Calculate Mean-Square-Logarithmic Error (Deviation)
#' 
#' For the ith sample, Squared Logarithmic Error is calculated as SLE = (log(prediction + 1) - log(actual + 1))^2. 
#' MSE is then mean(squared logarithmic errors). Note the '+1' in the calculation of SLE which avoids taking the logarithm of 0
#' for data which may include 0s.
#'
#' @details
#' Calculate Mean-Square-Logarithmic Error (Deviation)
#' 
#' @param preds A vector of prediction values in [0, 1]
#' @param actuals A vector of actuals values in {0, 1}, or {FALSE, TRUE}
#' @param weights Optional vectors of weights
#' @param na.rm Should (prediction, actual) pairs with at least one NA value be ignored?
#'
#' @examples
#' preds <- c(1.0, 2.0, 9.5)
#' actuals <- c(0.9, 2.1, 10.0)
#' msle(preds, actuals)
#' 
#' @export
#' @import data.table

msle <- function(preds = NULL, actuals = NULL, weights = 1, na.rm = FALSE){
  # mean-square-logarithmic error
  
  if(any(preds <= -1, na.rm = T)) stop("Can't calculate msle because some preds <= -1")
  if(any(actuals <= -1, na.rm = T)) stop("Can't calculate msle because some actuals <= -1")
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  Pred <- NULL
  Actual <- NULL
  Weight <- NULL
  
  #--------------------------------------------------
  
  if(is.logical(weights))
    stop("weights given as logical but should be numeric")
  
  if(!is.logical(na.rm))
    stop("na.rm should be logical")
  
  if(length(weights) > 1 & length(weights) != length(preds))
    stop("weights should be the same length as preds")
  
  if(na.rm == TRUE){
    dt <- data.table::data.table(Pred = preds, Actual = actuals, Weight = weights)
    result <- dt[!(is.na(Pred) | is.na(Actual)), list(Score = sum(Weight * (log1p(Pred) - log1p(Actual))^2)/sum(Weight))]$Score
  } else{
    if(length(weights) == 1) weights <- rep(weights, length(actuals))
    result <- weighted.mean((log1p(preds) - log1p(actuals))^2, w = weights)
  }
  
  return(result)
}
