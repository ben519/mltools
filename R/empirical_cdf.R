#' @title
#' Empirical Cumulative Distribution Function
#'
#' @description
#' Given a vector x, calculate P(x <= X) for a set of upper bounds X.
#' Can be applied to a data.table object for multivariate use. That is, 
#' calculate P(x <= X, y <= Y, z <= Z, ...)
#'
#' @details
#' Calculate the empirical CDF of a vector. Alternatively, leave \code{x} blank 
#' and pass a named list of vectors for \code{ubounds} to return a grid of upper 
#' bounds that is the cartesian product of the vectors in \code{ubounds}
#' 
#' @param x Numeric vector or a data.table object
#' @param ubounds A vector of upper bounds on which to evaluate the CDF.
#' For multivariate version, a list whose names correspond to columns of x
#'
#' @examples
#' library(data.table)
#' dt <- data.table(x=c(0.3, 1.3, 1.4, 3.6), y=c(1.2, 1.2, 3.8, 3.9))
#' empirical_cdf(dt$x, ubounds=as.numeric(1:4))
#' empirical_cdf(dt, ubounds=list(x=as.numeric(1:4)))
#' empirical_cdf(dt, ubounds=list(x=as.numeric(1:4), y=as.numeric(1:4)))
#' empirical_cdf(ubounds=list(x=as.numeric(1:4), y=as.numeric(1:4), z=as.numeric(1:2)))
#' 
#' @export
#' @import data.table

empirical_cdf <- function(x=NULL, ubounds){
  # Build the empirical_cdf of the given data
  # CDF points are the points given by ubounds, and if ubounds is a list of
  # bounds, the CDF points are the cartesian product of vectors in ubounds
  # x can be a numeric vector or a data.table
  # If x is a vector, ubounds should be a vector
  # If x is a data.table, ubounds should be a named list of vectors
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  BoundID <- NULL
  N <- NULL
  N.cum <- NULL
  
  #--------------------------------------------------
  
  # Build uboundsDT (cartesian product of all passed ubounds)
  if(mode(ubounds) != "list") ubounds <- list(UpperBound=ubounds)
  
  # If x is NULL, simply return uboundsDT
  if(is.null(x)){
    uboundsDT <- do.call(CJ, ubounds)[, BoundID := .I]
    setcolorder(uboundsDT, unique(c("BoundID", colnames(uboundsDT))))
    return(uboundsDT[])
  }
  
  # If x is a vector
  if(mode(x) == "numeric"){
    
    # Make sure ubounds's names are appropriate
    if(length(ubounds) > 1) stop("ubounds is a list of more than 1 element, but x is a vector")
    if(names(ubounds) == "") names(ubounds) <- "UpperBound"
    
    # Convert x to a data.table object
    xDT <- data.table(x)
    setnames(xDT, names(ubounds))
    
    # recursion
    return(empirical_cdf(xDT, ubounds))
  }
  
  #--------------------------------------------------
  # From here on, assume x is a data.table
  
  # Make sure the names of the ubounds list are all columns of x
  if(length(setdiff(names(ubounds), colnames(x))) > 0) stop("ubounds doesn't correspond to the columns in x")
  
  # Build the grid of upper bounds
  for(bound in names(ubounds)) ubounds[[bound]] <- sort(ubounds[[bound]])  # Force sorted bounds
  uboundsDT <- do.call(CJ, ubounds)
  
  # Build a copy of x
  binned <- copy(x[, names(uboundsDT), with=FALSE])
  
  # For each binning column, match each row of x to the nearest boundary above
  for(col in names(uboundsDT)){
    uboundDT <- data.table(ubounds[[col]], ubounds[[col]]); setnames(uboundDT, c(col, paste0("Bound.", col)))
    binned <- uboundDT[binned, on=col, roll=-Inf, nomatch=0]
  }
  
  # Aggregate to unique (BoundHitSpeed, BoundHLA, BoundVLA) tuples
  binned.uniques <- binned[, .N, keyby=eval(paste0("Bound.", names(ubounds)))]
  setnames(binned.uniques, paste0("Bound.", names(ubounds)), names(ubounds))
  
  # Get the count of samples directly below EVERY bound
  uboundsDT <- binned.uniques[uboundsDT, on=names(ubounds)]
  uboundsDT[is.na(N), N := 0]
  
  # Counting (see http://stackoverflow.com/a/40583817/2146894)
  if(length(ubounds) == 1){
    uboundsDT[, N.cum := cumsum(N)]
  } else{
    fixedCols <- names(ubounds)[-1]
    uboundsDT[, N.cum := cumsum(N), by=fixedCols]
    
    for(i in seq_len(length(ubounds) - 1)){
      i = i + 1
      fixedCols <- names(ubounds)[-i]
      uboundsDT[, N.cum := cumsum(N.cum), by=fixedCols]
    }
  }
  
  # Cleanup
  samples <- nrow(x)
  uboundsDT[, `:=`(N = NULL, CDF = N.cum/samples)]
  
  return(uboundsDT[])
}
