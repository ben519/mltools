#' @title
#' Mean Square Error
#'
#' @description
#' Calculate Mean-Square Error (Deviation)
#' 
#' For the ith sample, Squared Error is calculated as SE = (prediction - actual)^2. MSE is then mean(squared errors).
#' 
#' @details
#' Calculate Mean-Square Error (Deviation)
#' 
#' @param preds A vector of prediction values in [0, 1]
#' @param actuals A vector of actuals values in {0, 1}, or {FALSE, TRUE}
#' @param weights Optional vectors of weights
#' @param na.rm Should (prediction, actual) pairs with at least one NA value be ignored?
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Mean_squared_error}
#'
#' @examples
#' preds <- c(1.0, 2.0, 9.5)
#' actuals <- c(0.9, 2.1, 10.0)
#' mse(preds, actuals)
#' 
#' @export
#' @import data.table

mse <- function(preds = NULL, actuals = NULL, weights = 1, na.rm = FALSE){
  # mean-square error
  
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
    result <- dt[!(is.na(Pred) | is.na(Actual)), list(Score = sum(Weight * (Pred - Actual)^2)/sum(Weight))]$Score
  } else{
    if(length(weights) == 1) weights <- rep(weights, length(actuals))
    result <- weighted.mean((preds - actuals)^2, w = weights)
  }
  
  return(result)
}
