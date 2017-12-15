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
#'  \item{\code{x} and \code{y} can be given, leaving \code{preds} and \code{actuals} NULL}
#' }
#' 
#' @param preds A vector of predicted classes in \{0, 1, ...\}
#' @param actuals A vector of actual classes in \{0, 1, ...\}
#' @param x Indicator matrix of predicted classes (rows are samples, columns are classes). \eqn{x_{ij} = 1} if sample i is predicted as class j, 0 otherwise.
#' @param y Indicator matrix of actual classes (rows are samples, columns are classes). \eqn{y_{ij} = 1} if sample i is labelled as class j, 0 otherwise.
#'
#' @references
#' \url{https://doi.org/10.1371/journal.pone.0041882}
#' \url{https://doi.org/10.1016/j.compbiolchem.2004.09.006}
#' 

mcc <- function(preds=NULL, actuals=NULL, x=NULL, y=NULL) {
	# if preds and actuals are provided, x and y will be ignored
	if (!is.null(preds)) {
		nclasses <- length(union(preds, actuals))
		x <- matrix(0, nrow=length(preds), ncol=nclasses)
		y <- matrix(0, nrow=length(actuals), ncol=nclasses)
		x[cbind(1:nrow(x), preds+1)] <- 1
		y[cbind(1:nrow(y), actuals+1)] <- 1
	}
	if (!all(dim(x) == dim(y))) {
		stop("X and Y must have the same dimensions")
	}
	
	cov_biased <- function(x, y) {
		sum(sapply(1:ncol(x), function(k) {
			cov(x[,k], y[,k]) # unbiased estimate with (n-1) denominator as opposed to (n), but cancels out anyways so identical result
		}))
	}
	numerator <- cov_biased(x,y)
	denominator <- sqrt(cov_biased(x,x) * cov_biased(y,y))
	retval <- numerator / denominator	
	return(retval)
}




