#' @title
#' Mean Square Error
#'
#' @description
#' Calculate Mean-Square Error (Deviation)
#'
#' @details
#' Calculate Mean-Square Error (Deviation)
#' 
#' @param preds A vector of prediction values in [0, 1]
#' @param actuals A vector of actuals values in {0, 1}, or {FALSE, TRUE}
#' @param na.rm Should (prediction, actual) pairs with at least one NA value be ignored?
#' @param weights Optional vectors of weights
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

mse <- function(preds=NULL, actuals=NULL, na.rm=FALSE, weights = 1){
  # mean-square error
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  Pred <- NULL
  Actual <- NULL
  Weight <- NULL
  
  #--------------------------------------------------
  
  if(length(weights) > 1 & length(weights) != length(preds))
    stop("weights should be the same length as preds")
  
  if(na.rm == TRUE){
    dt <- data.table::data.table(Pred = preds, Actual = actuals, Weight = weights)
    result <- dt[!(is.na(Pred) | is.na(Actual)), list(Score = sum(Weight * (Pred - Actual)^2)/sum(Weight))]$Score
  } else{
    if(length(weights) == 1) weights <- rep(weights, length(preds))
    result <- weighted.mean((preds - actuals)^2, w = weights)
  }
  
  return(result)
}
