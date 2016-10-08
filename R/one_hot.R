#' @title
#' One Hot Encode
#'
#' @description
#' One-Hot-Encode unordered factor columns of a data.table (TODO: make it work on non-factor columns)
#'
#' @details
#' One-hot-encoding converts an unordered categorical vector (i.e. a factor) to multiple binarized vectors where each binary vector of
#' 1s and 0s indicates the presence of a class (i.e. level) of the of the original vector.
#'
#' @param dt A data.table
#' @param cols Which column(s) should be one-hot-encoded? DEFAULT = "auto" encodes all unordered factor columns
#' @param dropCols Should the resulting data.table exclude the original columns which are one-hot-encoded?
#' @param dropUnusedLevels Should columns of all 0s be generated for unused factor levels?
#'
#' @examples
#' library(data.table)
#' 
#' # test dataset
#' foo <- data.table(
#' ID=1:5, 
#' Color=factor(c("green", "red", "red", "blue", "green"), levels=c("blue", "green", "red", "purple")),
#' Shape=factor(c("square", "triangle", "square", "triangle", "cirlce"))
#' )
#' 
#' one_hot(foo)
#' one_hot(foo, dropCols=TRUE)
#' one_hot(foo, dropCols=FALSE)
#' one_hot(foo, dropUnusedLevels=TRUE)
#'
#' @export
#' @import data.table

one_hot <- function(dt, cols="auto", dropCols=TRUE, dropUnusedLevels=FALSE){
  # One-Hot-Encode unordered factors in a data.table
  # If cols = "auto", each unordered factor column in dt will be encoded. (Or specifcy a vector of column names to encode)
  # If dropCols=TRUE, the original factor columns are dropped
  # If dropUnusedLevels = TRUE, unused factor levels are dropped
  
  # Automatically get the unordered factor columns
  if(cols[1] == "auto") cols <- colnames(dt)[which(sapply(dt, function(x) is.factor(x) & !is.ordered(x)))]
  
  # Build tempDT containing and ID column and 'cols' columns
  tempDT <- dt[, cols, with=FALSE]
  tempDT[, ID := .I]
  setcolorder(tempDT, unique(c("ID", colnames(tempDT))))
  for(col in cols) set(tempDT, j=col, value=factor(paste(col, tempDT[[col]], sep="_"), levels=paste(col, levels(tempDT[[col]]), sep="_")))
  
  # One-hot-encode
  if(dropUnusedLevels == TRUE){
    newCols <- dcast(melt(tempDT, id = 'ID', value.factor = T), ID ~ value, drop = T, fun = length)
  } else{
    newCols <- dcast(melt(tempDT, id = 'ID', value.factor = T), ID ~ value, drop = F, fun = length)
  }
  
  # Combine binarized columns with the original dataset
  result <- cbind(dt, newCols[, !"ID"])
  
  # If dropCols = TRUE, remove the original factor columns
  if(dropCols == TRUE){
    result <- result[, !cols, with=FALSE]
  }
  
  return(result)
}
