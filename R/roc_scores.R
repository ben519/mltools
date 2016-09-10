#' Get ROC scores
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

  overall <- mean(actuals)
  dt <- data.table(Pred=preds, Actual=actuals)
  dt[, ROCPosition := (frank(Pred) - 1) / (.N - 1)]
  dt[, Err := abs(ROCPosition - overall)]
  dt[(Actual==0 & ROCPosition <= overall) | (Actual==1 & ROCPosition >= overall), Err := 0]

  return(dt$Err)
}
