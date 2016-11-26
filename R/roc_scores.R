#' @title
#' ROC scores
#' 
#' @description
#' This function provides a way to identify the worst predictions when measuring Area Under the ROC curve. Simply
#' put, the worst predictions are the ones with very low or high relative prediction scores (usually probabilities) 
#' which relate to the positive and negative samples respectively.
#' 
#' @details
#' How it works
#' \itemize{
#' \item{First the relative position (between 0 and 1) of each prediction is determined}
#' \item{Next the mean of actuals is determined}
#' \item{For samples whose position is on the correct side of the overall mean, 0 is given}
#' \item{For samples whose position is on the wrong side of the overall mean, its distance from the mean is given}
#' }
#' 
#' @param preds vector of predictions (need not be in range [0-1] - only order matters)
#' @param actuals vector of actuals - either logical or vector of 1s and 0s
#'
#' @examples
#' roc_scores(c(1,2,3,4), actuals=c(1,1,0,0))
#' roc_scores(c(0.1, 0.2, 0.3, 0.4), actuals=c(TRUE, FALSE, TRUE, FALSE))
#'
#' @export
#' @import data.table

roc_scores <- function(preds, actuals){
  # Returns a score for each prediction (between 0 (good) and 1 (bad))
  
  # How it works:
  # First the relative position (between 0 and 1) of each prediction is determined
  # Next the mean of actuals is determined
  # For samples whose position is on the correct side of the overall mean, 0 is given
  # For samples whose position is on the wrong side of the overall mean, its distance from the mean is given
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  ROCPosition <- NULL
  Pred <- NULL
  Err <- NULL
  Actual <- NULL
  Count <- NULL
  Value <- NULL
  
  #--------------------------------------------------
  
  overall <- mean(actuals)
  dt <- data.table(Pred=preds, Actual=actuals)
  dt[, ROCPosition := (frank(Pred) - 1) / (.N - 1)]
  dt[, Err := abs(ROCPosition - overall)]
  dt[(Actual==0 & ROCPosition <= overall) | (Actual==1 & ROCPosition >= overall), Err := 0]
  
  return(dt$Err)
}
