#' @title
#' Map a vector of numeric values into bins
#'
#' @description
#' Takes a vector of values and bin parameters and maps each value to an ordered factor whose levels are a set of bins like [0,1), [1,2), [2,3).
#'
#' Values may be provided as a vector or via a pair of parameters - a data.table object and the name of the column to bin.
#'
#' @details
#' This function can return two different types of output, depending on whether \code{returnDT} is \code{TRUE} or \code{FALSE}.
#'
#' If \code{returnDT=FALSE}, returns an ordered factor vector of bins like [1, 2), [-3,-2), ... corresponding to the values which were
#' binned and whose levels correspond to all the generated bins. (Note that empty bins may be present as unused factor levels).
#'
#' If \code{returnDT=TRUE}, returns a data.table object with all values and all bins (including empty bins). If \code{dt} is provided
#' instead of \code{vals}, a full copy of \code{dt} is created and merged with the set of generated bins.
#'
#' @param x A vector of values or a data.table object
#' @param binCol A column of \code{dt} specifying the values to bin
#' @param bins
#' \itemize{
##'  \item{\bold{integer}} {specifying the number of bins to generate}
##'  \item{\bold{numeric vector}} {specifying sequential bin boundaries \{(x0, x1), (x1, x2), ..., (xn-1, xn)\}}
##'  \item{\bold{2-column data.frame/data.table}} {each row defines a bin}
##' }
#'
#' @param binType
#' \itemize{
##'  \item{\bold{"explicit"}} {interpret bins as they are given}
##'  \item{\bold{"quantile"}} {interpret bins as quantiles (empty quantile bins will be discarded)}
##' }
#'
#' @param boundaryType
#' \itemize{
##'  \item{\bold{"lcro]"}} {bins are [left-closed, right-open) except for last bin which is [left-closed, right-closed]}
##'  \item{\bold{"lcro)"}} {bins are [left-closed, right-open)}
##'  \item{\bold{"[lorc"}} {bins are (left-open, right-closed] except for first bin which is [left-closed, right-closed]}
##'  \item{\bold{"(lorc"}} {bins are (left-open, right-closed]}
##' }
#'
#' @param returnDT If \bold{FALSE}, return an ordered factor of bins corresponding to the values given, else return
#' a data.table object which includes all bins and values (makes a copy of data.table object if given)
#'
#' @examples
#' library(data.table)
#' iris.dt <- data.table(iris)
#' 
#' # custom bins
#' bin_data(iris.dt, binCol="Sepal.Length", bins=c(4, 5, 6, 7, 8))
#' 
#' # 10 equally spaced bins
#' bin_data(iris$Petal.Length, bins=10, returnDT=TRUE)
#' 
#' # make the last bin [left-closed, right-open)
#' bin_data(c(0,0,1,2), bins=2, boundaryType="lcro)", returnDT=TRUE)
#' 
#' # bin values by quantile
#' bin_data(c(0,0,0,0,1,2,3,4), bins=4, binType="quantile", returnDT=TRUE)
#'
#' @export
#' @import data.table
#' @importFrom stats quantile

bin_data <- function(x=NULL, binCol=NULL, bins=10, binType="explicit", boundaryType="lcro]", returnDT=FALSE){
  # Bin a vector of values

  # Two call formats:
  # Provide a vector of values to be binned or
  # bins can be a single integer, a vector of numbers, or a 2-column data.frame/data.table specifiying LBs and RBs
  # binType is one of {"explicit", "quantile"}
  # boundaryType is one of {"lcro]", "lcro)", "[lorc", "(lorc"} (i.e. "left-closed right-open" or "left-open right-closed",
  #  where last open boundary is closed ']' or open ')' or similarly the first open boundary can be closed '[' or open '('.)
  # If returnDT is FALSE, a return an ordered factor values [LB, RB) corresponding to each element of dt,
  #  else return dt with an added Bin column and potential extra rows (i.e. empty bins)
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  LB <- NULL
  RB <- NULL
  Bin <- NULL
  BinCol <- NULL
  i.Bin <- NULL
  
  #--------------------------------------------------
  
  if(is(x, "data.table") & is.null(binCol)) stop("binCol must be given")
  if(!is(x, "data.table") & !is.null(binCol)) stop("You specified binCol but didn't provided a data.table object for x")

  #--------------------------------------------------
  # Build binDT

  if(is(x, "data.table")) vals <- as.numeric(x[[binCol]]) else vals <- as.numeric(x)

  # Get the bin values
  if(binType == "explicit"){
    if(is(bins, "data.frame")){
      LBs <- bins[[1]]
      RBs <- bins[[2]]
    }else if(length(bins) == 1){
      binVals <- seq(min(vals), max(vals), length.out=bins+1)
      LBs <- head(binVals, -1)
      RBs <- tail(binVals, -1)
    } else{
      LBs <- head(bins, -1)
      RBs <- tail(bins, -1)
    }
  }
  if(binType == "quantile"){
    if(is(bins, "data.frame")){
      LBs <- quantile(vals, probs=bins[[1]], na.rm=TRUE)
      RBs <- quantile(vals, probs=bins[[2]], na.rm=TRUE)
    }else if(length(bins) == 1){
      binVals <- unique(quantile(vals, probs=seq(0, 1, length.out=bins+1), na.rm=TRUE))
      LBs <- head(binVals, -1)
      RBs <- tail(binVals, -1)
    } else{
      LBs <- head(unique(quantile(vals, probs=bins, na.rm=TRUE)), -1)
      RBs <- tail(unique(quantile(vals, probs=bins, na.rm=TRUE)), -1)
    }
  }

  # Build binDT
  binDT <- data.table(LB=LBs, RB=RBs)[order(LB, RB)]

  # Set the Bin column
  if(boundaryType == "lcro]"){
    binDT[, Bin := paste0("[", LB, ", ", RB, ")")]
    binDT[nrow(binDT), Bin := paste0("[", LB, ", ", RB, "]")]
    binDT[, Bin := factor(Bin, levels=Bin, ordered=TRUE)]
  } else if(boundaryType == "lcro)"){
    binDT[, Bin := paste0("[", LB, ", ", RB, ")")]
    binDT[, Bin := factor(Bin, levels=Bin, ordered=TRUE)]
  } else if(boundaryType == "[lorc"){
    binDT[, Bin := paste0("(", LB, ", ", RB, "]")]
    binDT[1, Bin := paste0("[", LB, ", ", RB, "]")]
    binDT[, Bin := factor(Bin, levels=Bin, ordered=TRUE)]
  } else if(boundaryType == "(lorc"){
    binDT[, Bin := paste0("(", LB, ", ", RB, "]")]
    binDT[, Bin := factor(Bin, levels=Bin, ordered=TRUE)]
  }

  #--------------------------------------------------
  # Determine the Bin for each row in dt

  binData <- data.table(BinCol=vals)

  if(boundaryType == "lcro]"){
    binData[binDT, on=list(BinCol >= LB, BinCol < RB), Bin := Bin]
    binData[tail(binDT, 1), on=list(BinCol >= LB, BinCol <= RB), Bin := i.Bin]
  } else if(boundaryType == "lcro)"){
    binData[binDT, on=list(BinCol >= LB, BinCol < RB), Bin := Bin]
  } else if(boundaryType == "[lorc"){
    binData[binDT, on=list(BinCol > LB, BinCol <= RB), Bin := Bin]
    binData[head(binDT, 1), on=list(BinCol >= LB, BinCol <= RB), Bin := i.Bin]
  } else if(boundaryType == "(lorc"){
    binData[binDT, on=list(BinCol > LB, BinCol <= RB), Bin := Bin]
  }

  #--------------------------------------------------
  # Return the desired result

  if(returnDT == FALSE){
    # If returnDT is FALSE, return the vector of bins corresponding to the rows of x

    return(binData$Bin)
  } else{
    # Else return a data.table object with all bin values and potentially extra rows (empty bins)

    baseDT <- data.table(BinVal=vals)
    baseDT[, Bin := binData$Bin]  # set the bins
    binnedData <- merge(binDT[, list(Bin)], baseDT, all=TRUE)  # full outer join with binDT

    return(binnedData)
  }
}
