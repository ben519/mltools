#' Group data.table into bins
#'
#' @param vals A vector of values
#' @param dt A data.table object
#' @param binCol A a column of \code{dt} specifying the values to bin
#' @param bins One of: \cr
#'   - single integer specifying number of bins to generate
#'   - vector of numbers specifying sequential bin boundaries {(x0, x1), (x1, x2), ..., (xn-1, xn)}
#'   - 2-column data.frame/data.table specifiying left-bounds and right-bounds
#' @param binType One of: \cr
#'   - "explicit" - generate bin values as they are given
#'   - "quantile" - generate bins based on quantiles (empty quantile bins will be discarded) ... needs improvement
#' @param boundaryType One of: \cr
#'   - "lcro]" - bins are [left-closed, right-open) except for last bin which is (left-closed, right-closed]
#'   - "lcro)" - bins are [left-closed, right-open)
#'   - "[lorc" - bins are (left-open, right-closed] except for first bin which is [left-closed, right-closed]
#'   - "(lorc" - bins are (left-open, right-closed]
#' @param returnDT If FALSE, return an ordered factor of bins corresponding to the values given, else return \cr
#' a data.table object which includes all bins and values (makes a copy of \code{dt} if given)
#'
#' @examples
#' iris.dt <- data.table(iris)
#' bin_data(dt=iris.dt, binCol="Sepal.Length", bins=c(4, 5, 6, 7, 8))
#' bin_data(vals=iris$Petal.Length, bins=10, returnDT=TRUE)  # 10 equally spaced bins
#' bin_data(vals=c(0,0,1,2), bins=2, boundaryType="lcro)", returnDT=TRUE)  # make the last bin [left-closed, right-open)
#' bin_data(vals=c(0,0,0,0,1,2,3,4), bins=4, binType="quantile", returnDT=TRUE)  # bin values by quantile
#'
#' @export
#' @import data.table

bin_data <- function(vals=NULL, dt=NULL, binCol=NULL, bins=10, binType="explicit", boundaryType="lcro]", returnDT=FALSE){
  # Bin a vector of values

  # Two call formats:
  # Provide a vector of values (vals) to be binned or
  # Provide a data.table (dt) object and specify the column of values to be binned (binCol)
  # bins can be a single integer, a vector of numbers, or a 2-column data.frame/data.table specifiying LBs and RBs
  # binType is one of {"explicit", "quantile"}
  # boundaryType is one of {"lcro]", "lcro)", "[lorc", "(lorc"} (i.e. "left-closed right-open" or "left-open right-closed",
  #  where last open boundary is closed ']' or open ')' or similarly the first open boundary can be closed '[' or open '('.)
  # If returnDT is FALSE, a return an ordered factor values [LB, RB) corresponding to each element of dt,
  #  else return dt with an added Bin column and potential extra rows (i.e. empty bins)

  #--------------------------------------------------
  # Build binDT

  if(is.null(vals)) vals <- dt[[binCol]]

  # Get the bin values
  if(binType == "explicit"){
    if(is(bins, "data.frame")){
      LBs <- bins[, 1]
      RBs <- bins[, 2]
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
      LBs <- quantile(vals, probs=bins[, 1])
      RBs <- quantile(vals, probs=bins[, 2])
    }else if(length(bins) == 1){
      binVals <- unique(quantile(vals, probs=seq(0, 1, length.out=bins+1)))
      LBs <- head(binVals, -1)
      RBs <- tail(binVals, -1)
    } else{
      LBs <- head(unique(quantile(vals, probs=bins)), -1)
      RBs <- tail(unique(quantile(vals, probs=bins)), -1)
    }
  }

  # Build binDT
  binDT <- data.table(LB=LBs, RB=RBs)

  # Set the Bin column
  if(boundaryType == "lcro]"){
    binDT[, Bin := paste0("[", LB, ", ", RB, ")")]
    binDT[nrow(binDT), Bin := paste0("[", LB, ", ", RB, "]")]
    binDT[, Bin := factor(Bin, ordered=TRUE)]
  } else if(boundaryType == "lcro)"){
    binDT[, Bin := paste0("[", LB, ", ", RB, ")")]
    binDT[, Bin := factor(Bin, ordered=TRUE)]
  } else if(boundaryType == "[lorc"){
    binDT[, Bin := paste0("(", LB, ", ", RB, "]")]
    binDT[1, Bin := paste0("[", LB, ", ", RB, "]")]
    binDT[, Bin := factor(Bin, ordered=TRUE)]
  } else if(boundaryType == "(lorc"){
    binDT[, Bin := paste0("(", LB, ", ", RB, "]")]
    binDT[, Bin := factor(Bin, ordered=TRUE)]
  }

  #--------------------------------------------------
  # Determine the Bin for each row in dt

  if(is.null(vals)) binData <- dt[, list(BinCol=get(binCol))] else binData <- data.table(BinCol=vals)

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
    # If returnDT is FALSE, return the vector of bins corresponding to the rows of dt

    return(binData$Bin)
  } else{
    # Else return a data.table object with all bin values and potentially extra rows (empty bins)

    if(is.null(dt)) baseDT <- data.table(BinVal=vals) else baseDT <- copy(dt)
    baseDT[, Bin := binData$Bin]  # set the bins
    binnedData <- merge(binDT[, list(Bin)], baseDT, all=TRUE)  # full outer join with binDT

    return(binnedData)
  }
}
