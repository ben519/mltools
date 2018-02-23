#' @title
#' Matthews correlation coefficient
#'
#' @description
#' Calculate Matthews correlation coefficient, with support for the multi-class definition:
#' \deqn{MCC = \frac{cov(X,Y)}{\sqrt{cov(X,X)*cov(Y,Y)}}}{MCC = cov(X,Y) / sqrt(cov(X,X)*cov(Y,Y))}
#'
#' @details
#' Calculate Matthews correlation coefficient. Either
#' 
#' \itemize{
#'  \item{\code{preds} and \code{actuals} can be given, in which case \code{x} and \code{y} will be ignored} or
#'  \item{\code{x} and \code{y} can be given, leaving \code{preds} and \code{actuals} NULL} or
#'	\item{\code{TP}, \code{FP}, \code{TN}, and \code{FN}, leaving all other arguments NULL}
#' }
#' 
#' @param preds A vector of predicted classes in \{0, 1, ...\}
#' @param actuals A vector of actual classes in \{0, 1, ...\}
#' @param x Indicator matrix of predicted classes (rows are samples, columns are classes). \eqn{x_{ij} = 1} if sample i is predicted as class j, 0 otherwise.
#' @param y Indicator matrix of actual classes (rows are samples, columns are classes). \eqn{y_{ij} = 1} if sample i is labelled as class j, 0 otherwise.
#' @param TP Count of true positives (correctly predicted 1/TRUE)
#' @param FP Count of false positives (predicted 1/TRUE, but actually 0/FALSE)
#' @param TN Count of true negatives (correctly predicted 0/FALSE)
#' @param FN Count of false negatives (predicted 0/FALSE, but actually 1/TRUE)
#'
#' @references
#' \url{https://doi.org/10.1371/journal.pone.0041882} \cr
#' \url{https://doi.org/10.1016/j.compbiolchem.2004.09.006}
#' 

mcc <- function(preds=NULL, actuals=NULL, x=NULL, y=NULL, TP=NULL, FP=NULL, TN=NULL, FN=NULL) {
	input_type <- NULL
	# if preds and actuals are provided, x and y will be ignored
	if (!is.null(preds) & !is.null(actuals)) {
		if (length(preds) != length(actuals)) {
			stop("preds and actuals must be vectors of the same length")
		}
		nclasses <- length(union(unique(preds), unique(actuals)))
		if (nclasses > 2) {
			x <- matrix(0, nrow=length(preds), ncol=nclasses)
			y <- matrix(0, nrow=length(actuals), ncol=nclasses)
			x[cbind(1:nrow(x), preds+1)] <- 1
			y[cbind(1:nrow(y), actuals+1)] <- 1
			input_type <- "multiclass"
		} else {
			input_type <- "twoclass"
		}
	} else if (!is.null(x) & !is.null(y)) {
		if (!all(dim(x) == dim(y))) {
			stop("X and Y must have the same dimensions")
		}
		input_type <- "multiclass"
	} else if (!is.null(TP) & !is.null(FP) & !is.null(TN) & !is.null(FN)) {
		input_type <- "2x2"
	} else {
		stop("Some input must be provided")
	}
	if (input_type == "multiclass") {
#		cov_biased <- function(x, y) {
#			sum(sapply(1:ncol(x), function(k) {
#				cov(x[,k], y[,k]) # unbiased estimate with (n-1) denominator as opposed to (n), but cancels out anyways so identical result
#			}))
#		}
		cov_biased <- function(x, y) {
			sum(diag(cov(x,y)))
		}
		
		numerator <- cov_biased(x,y)
		denominator <- sqrt(cov_biased(x,x) * cov_biased(y,y))
		retval <- numerator / denominator	
	} else if (input_type %in% c("twoclass", "2x2")) {
		if (input_type == "twoclass") {
	    TP <- sum(actuals[preds==1] == 1)
		  FP <- sum(actuals[preds==1] == 0)
		  TN <- sum(actuals[preds==0] == 0)
		  FN <- sum(actuals[preds==0] == 1)
		}
		TP <- as.double(TP)
		FP <- as.double(FP)
		TN <- as.double(TN)
		FN <- as.double(FN)
		numerator <- (TP * TN - FP * FN)
		denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
		if (denominator == 0) {
			denominator <- 1
		}
		retval <- numerator/denominator
	}
	return(retval)
}



