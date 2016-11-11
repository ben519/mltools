#' @title
#' Matthews correlation coefficient
#'
#' @description
#' Calculate Matthews correlation coefficient
#'
#' @details
#' Calculate Matthews correlation coefficient. Either
#' 
#' \itemize{
#'  \item{\code{preds} and \code{actuals} can be given, leaving \code{TP}, \code{FP}, \code{TN}, and \code{FN} NULL} or
#'  \item{\code{TP}, \code{FP}, \code{TN}, and \code{FN} can be given, leaving \code{preds} and \code{actuals} NULL}
#' }
#' 
#' @param preds A vector of prediction values in {1, 0}, or {TRUE, FALSE}
#' @param actuals A vector of actuals values in {1, 0}, or {TRUE, FALSE}
#' @param TP Count of true positives (correctly predicted 1/TRUE)
#' @param FP Count of false positives (predicted 1/TRUE, but actually 0/FALSE)
#' @param TN Count of true negatives (correctly predicted 0/FALSE)
#' @param FN Count of false negatives (predicted 0/FALSE, but actually 1/TRUE)
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Matthews_correlation_coefficient}
#'
#' @examples
#' preds <- c(1,1,1,0,1,1,0,0)
#' actuals <- c(1,1,1,1,0,0,0,0)
#' mcc(preds, actuals)
#' mcc(actuals, actuals)
#' mcc(TP=3, FP=2, TN=2, FN=1)
#' 
#' @export
#' @import data.table

mcc <- function(preds=NULL, actuals=NULL, TP=NULL, FP=NULL, TN=NULL, FN=NULL){
  # Matthews correlation coefficient
  # Either preds and actuals should be given or TP, FP, TN, and FN should be given
  # preds and actuals should be vectors with values in {0, 1} or {TRUE, FALSE}
  # TP, FP, TN, FN should be values representing the count of True Positives, False Positives, True Negatives, False Negatives
  
  valid_input <- FALSE
  if(is.null(preds) & is.null(actuals) & (!is.null(TP) & !is.null(FP) & !is.null(TN) & !is.null(FN))) valid_input <- TRUE
  if(!is.null(preds) & !is.null(actuals) & (is.null(TP) & is.null(FP) & is.null(TN) & is.null(FN))) valid_input <- TRUE
  if(!valid_input) stop("Either preds and actuals should be provided or TP, FP, TN, and FN should be provided")
  
  # If preds/actuals provided, measure TP, FP, TN, FN
  if(!is.null(preds)){
    
    # Check input values
    if(sum(!preds %in% c(0, 1)) + sum(!actuals %in% c(0, 1)) > 0) stop("preds and actuals must be vectors of {1, 0} or {TRUE, FALSE}")
    
    TP <- sum(actuals[preds==1] == 1)
    FP <- sum(actuals[preds==1] == 0)
    TN <- sum(actuals[preds==0] == 0)
    FN <- sum(actuals[preds==0] == 1)
  }
  
  # Convert types to double for better precision
  TP <- as.double(TP)
  FP <- as.double(FP)
  TN <- as.double(TN)
  FN <- as.double(FN)
  
  # Calculate MCC
  numerator <- (TP * TN - FP * FN)
  denominator <- sqrt((TP + FP)*(TP + FN)*(TN + FP)*(TN + FN))
  if(denominator == 0) denominator <- 1
  mcc <- numerator/denominator
  
  return(mcc)
}