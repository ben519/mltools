#' Group data.table into bins
#'
#' @param dt A data.table
#' @param col A a column of \code{dt}
#' @param bins A vector of binning boundaries
#' @param aggregate Should the data be aggregated per bin?
#'
#' @return If \code{aggregate} = TRUE, returns \code{dt} grouped on \code{col} by the specified \code{bins} \cr
#' taking averages for all other numeric columns. Otherwise returns \code{dt} with bin identifications per row.
#'
#' @examples
#' iris.dt <- data.table(iris)
#' bin_data(dt=iris.dt, col="Sepal.Length", bins=c(4, 5, 6, 7, 8), aggregate=TRUE)
#'
#' @export
#' @import data.table

bin_data <- function(dt, col, bins, aggregate=TRUE){
  # Bins the data in dt by the specified column, left-closed, right-open
  # Bins should be like c(-Inf, 0, 10, 100, Inf) to guarantee extremes are captured

  # Determine which columns in dt are numeric (averages will be take for each of of these columns within each bin)
  numericCols <- colnames(dt)[sapply(dt, function(x) is.numeric(x) | is.logical(x))]

  # Build a table in the shape of the final output
  binned <- data.table(LB.closed=head(bins, -1), RB.open=tail(bins, -1))

  # Temporarily add a column to binned: col
  binned[, eval(parse(text=paste0(col, ":=LB.closed")))]

  # Add a Bin column = [LB.closed, RB.open) to binned
  binLabels <- paste0("[", binned$LB.closed, ",", binned$RB.open, ")")
  binned[, Bin := factor(paste0("[", LB.closed, ",", RB.open, ")"), levels=binLabels, ordered=TRUE)]

  # Roll the bins onto dt using col (a copy of LB.closed) to identify which bin to map each rows of dt to
  temp <- binned[dt, roll=TRUE, on=col]
  temp <- temp[get(col) < RB.open] # exclude values which are outside the largest rightbound

  # Potentially return the non aggregated data
  if(!aggregate) return(temp)

  # Group by RB.closed taking averages for all numeric columns
  expr <- paste0(numericCols, ".mean=mean(", numericCols, ", na.rm=TRUE)", collapse=", ")
  expr <- paste0("list(.N, ", expr, ")")
  temp <- temp[, eval(parse(text=expr)), by=RB.open]  # group by RB.closed

  # Merge the results to binned
  result <- temp[binned[, list(Bin, LB.closed, RB.open)], on="RB.open"]

  # Reset the column order of binned
  setcolorder(result, unique(c("Bin", "LB.closed", "RB.open", colnames(result))))

  return(result)
}
