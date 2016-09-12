#' Area Under the ROC Curve
#'
#' @export
#' @import data.table
#'
#' @examples
#' preds <- c(.1, .3, .3, .9)
#' actuals <- c(0, 0, 1, 1)
#' auc_roc(preds, actuals)
#' auc_roc(preds, actuals, returnDT=TRUE)

auc_roc <- function(preds, actuals, returnDT=FALSE){
  # Calculate area under the ROC curve
  # If returnDT = TRUE, a data.table is returned

  # Check if every prediction is identical and if so, return 0.5
  if(length(unique(preds)) == 1) return(0.5)

  dt <- data.table(Pred=preds, Actual=actuals*1L)
  setorder(dt, -Pred)

  bg <- dt[, list(CountFalse=sum(Actual==0), CountTrue=sum(Actual)), by=list(Pred)]

  # Calculate the CumulativeFalsePositiveRate and CumulativeTruePositiveRate
  bg[, CumulativeFPR := cumsum(CountFalse)/sum(CountFalse)]
  bg[, CumulativeTPR := cumsum(CountTrue)/sum(CountTrue)]

  # Calculate AUC ROC
  bg[, AdditionalArea := c(head(CumulativeFPR, 1) * head(CumulativeTPR, 1)/2,
                           (tail(CumulativeFPR, -1) - head(CumulativeFPR, -1)) * (head(CumulativeTPR, -1) + (tail(CumulativeTPR, -1) - head(CumulativeTPR, -1))/2))]
  bg[, CumulativeArea := cumsum(AdditionalArea)]

  # Return the desired result
  if(returnDT) return(bg[]) else return(tail(bg$CumulativeArea, 1))
}
