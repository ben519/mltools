#' @title
#' Empirical Cumulative Distribution Function
#'
#' @description
#' Given a vector x, calculate P(x <= X) for a set of upper bounds X.
#' Can be applied to a data.table object for multivariate use. That is, 
#' calculate P(x <= X, y <= Y, z <= Z, ...)
#'
#' @details
#' Calculate the empirical CDF of a vector, or data.table with multiple columns for multivariate use.
#' 
#' @param x Numeric vector or a data.table object for multivariate use.
#' @param ubounds A vector of upper bounds on which to evaluate the CDF.
#' For multivariate version, a data.table whose names correspond to columns of x.
#'
#' @examples
#' library(data.table)
#' dt <- data.table(x=c(0.3, 1.3, 1.4, 3.6), y=c(1.2, 1.2, 3.8, 3.9))
#' empirical_cdf(dt$x, ubounds=1:4)
#' empirical_cdf(dt, ubounds=CJ(x = 1:4, y = 1:4))
#' 
#' @export
#' @import data.table

empirical_cdf <- function(x, ubounds){
  # Build the empirical_cdf of the given data
  # CDF points are the points given by ubounds
  # x can be a numeric vector or a data.table
  # ubounds can be a numeric vector or a data.table, but must correspond to x accordingly
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  BoundID <- NULL
  N <- NULL
  N.cum <- NULL
  
  #--------------------------------------------------
  # Check the input
  
  if(!is.numeric(x) & !is.data.table(x))
    stop("x must be numeric or a data.table object")
  
  if(!is.numeric(ubounds) & !is.data.table(ubounds))
    stop("ubounds must be numeric or a data.table object")
  
  if(is.data.table(ubounds)){
    if(ncol(ubounds) > 1){
      if(!is.data.table(x)){
        stop("If ubounds is a data.table with multiple columns, x should be a data.table object with corresponding columns")
      } else if(length(setdiff(colnames(ubounds), colnames(x))) > 0)
        stop("ubounds contains columns not found in x")
    }
  }
  
  if(is.data.table(x)){
    if(ncol(x) > 1 & !is.data.table(ubounds)){
      stop("If x is a data.table with multiple columns, ubounds must be a data.table with at least one column otherwise it is unclear which column of x ubounds relates to.")
    }
  }
  
  #--------------------------------------------------
  
  # Build ubounds
  if(!is.data.table(ubounds)){
    ubounds <- data.table(UpperBound = ubounds)
    if(is.data.table(x)) setnames(ubounds, "UpperBound", colnames(x))  # Here, colnames(x) should be one value
  }
  
  # If x is a vector
  if(mode(x) == "numeric"){
    
    # Convert x to a data.table object
    x <- data.table(x)
    setnames(x, names(ubounds))
  }
  
  #--------------------------------------------------
  # From here on, assume x is a data.table
  
  # Convert columns of x and ubounds to numeric. This is to fix a data.table bug involving rolling joins on integer columns
  x <- x[, colnames(ubounds), with=F]
  ubounds <- copy(ubounds)
  for(col in colnames(ubounds)){
    set(x, j=col, value=as.numeric(x[[col]]))
    set(ubounds, j=col, value=as.numeric(ubounds[[col]]))
  }
  
  # Reduce to uniques
  uboundsUniques <- unique(ubounds)
  
  # Build a copy of x
  binned <- copy(x[, names(uboundsUniques), with=FALSE])
  
  # For each binning column, match each row of x to the nearest boundary above
  for(col in names(uboundsUniques)){
    uboundDT <- data.table(uboundsUniques[[col]], uboundsUniques[[col]])
    setnames(uboundDT, c(col, paste0("Bound.", col)))
    binned <- uboundDT[binned, on=col, roll=-Inf, nomatch=0]
  }
  
  # Aggregate to unique tuples based on Bound.* columns
  binned.uniques <- binned[, .N, keyby=eval(paste0("Bound.", names(ubounds)))]
  setnames(binned.uniques, paste0("Bound.", names(ubounds)), names(ubounds))
  
  # Get the count of samples directly below EVERY bound
  uboundsUniques <- binned.uniques[uboundsUniques, on=names(ubounds)]
  uboundsUniques[is.na(N), N := 0]
  
  # Counting (see http://stackoverflow.com/a/40583817/2146894)
  if(length(ubounds) == 1){
    uboundsUniques[, N.cum := cumsum(N)]
  } else{
    fixedCols <- names(ubounds)[-1]
    uboundsUniques[, N.cum := cumsum(N), by=fixedCols]
    
    for(i in seq_len(length(ubounds) - 1)){
      i = i + 1
      fixedCols <- names(ubounds)[-i]
      uboundsUniques[, N.cum := cumsum(N.cum), by=fixedCols]
    }
  }
  
  # Cleanup
  samples <- nrow(x)
  uboundsUniques[, `:=`(N = NULL, CDF = N.cum/samples)]
  
  # Join back to ubounds in case of potential duplicate ubounds
  ubounds <- uboundsUniques[ubounds, on=names(ubounds)]
  
  return(ubounds[])
}
