#' @title
#' Area Under the ROC Curve
#'
#' @description
#' Calculates Area Under the ROC Curve
#'
#' @details
#' If \code{returnDT=FALSE}, returns Area Under the ROC Curve.If \code{returnDT=TRUE}, returns a data.table object with
#' False Positive Rate and True Positive Rate for plotting the ROC curve.
#' 
#' @param preds A vector of prediction values
#' @param actuals A vector of actuals values (numeric or ordered factor)
#' @param returnDT If TRUE, a data.table of (FalsePositiveRate, TruePositiveRate) pairs is returned, otherwise AUC ROC score is returned 
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Receiver_operating_characteristic#Area_under_the_curve}
#'
#' @export
#' @import data.table
#' @importFrom methods is
#'
#' @examples
#' library(data.table)
#' preds <- c(.1, .3, .3, .9)
#' actuals <- c(0, 0, 1, 1)
#' auc_roc(preds, actuals)
#' auc_roc(preds, actuals, returnDT=TRUE)

auc_roc <- function(preds, actuals, returnDT=FALSE){
  # Calculate area under the ROC curve
  # If returnDT = TRUE, a data.table is returned
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  Pred <- NULL
  Actual <- NULL
  CumulativeFPR <- NULL
  CountFalse <- NULL
  CumulativeTPR <- NULL
  CountTrue <- NULL
  AdditionalArea <- NULL
  CumulativeArea <- NULL
  
  #--------------------------------------------------
  
  # Check if every prediction is identical and if so, return 0.5
  if(length(unique(preds)) == 1L) return(0.5)
  
  # Convert actuals to numeric if it's an ordered factor
  if(is(actuals, "factor")){
    if(is.ordered(actuals) & length(levels(actuals)) == 2) actuals <- as.numeric(actuals) - 1 else stop("actuals is type factor, but is unordered. Make it an ordered factor.")
  }

  dt <- data.table(Pred=preds, Actual=actuals*1L)
  setorder(dt, -Pred)

  dt <- dt[ , {
      CountTrue = sum(Actual)
      list(CountFalse=.N - CountTrue, CountTrue=CountTrue)
    }, by=Pred]

  # Calculate the CumulativeFalsePositiveRate and CumulativeTruePositiveRate
  dt[, CumulativeFPR := cumsum(CountFalse)/sum(CountFalse)]
  dt[, CumulativeTPR := cumsum(CountTrue)/sum(CountTrue)]

  # Calculate AUC ROC
  dt[, AdditionalArea := c(head(CumulativeFPR, 1) * head(CumulativeTPR, 1)/2,
                           (tail(CumulativeFPR, -1) - head(CumulativeFPR, -1)) * (head(CumulativeTPR, -1) + (tail(CumulativeTPR, -1) - head(CumulativeTPR, -1))/2))]
  dt[, CumulativeArea := cumsum(AdditionalArea)]

  # Return the desired result
  if(returnDT) return(dt[]) else return(tail(dt$CumulativeArea, 1))
}
