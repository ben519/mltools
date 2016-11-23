#' @title
#' One Hot Encode
#'
#' @description
#' One-Hot-Encode unordered factor columns of a data.table
#'
#' @details
#' One-hot-encoding converts an unordered categorical vector (i.e. a factor) to multiple binarized vectors where each binary vector of
#' 1s and 0s indicates the presence of a class (i.e. level) of the of the original vector.
#'
#' @param dt A data.table
#' @param cols Which column(s) should be one-hot-encoded? DEFAULT = "auto" encodes all unordered factor columns
#' @param sparsifyNAs Should NAs be converted to 0s?
#' @param naCols Should columns be generated to indicate the present of NAs? Will only apply to factor columns with at least one NA
#' @param dropCols Should the resulting data.table exclude the original columns which are one-hot-encoded?
#' @param dropUnusedLevels Should columns of all 0s be generated for unused factor levels?
#'
#' @examples
#' library(data.table)
#' 
#' dt <- data.table(
#'   ID = 1:4,
#'   color = factor(c("red", NA, "blue", "blue"), levels=c("blue", "green", "red"))
#' )
#' 
#' one_hot(dt)
#' one_hot(dt, sparsifyNAs=TRUE)
#' one_hot(dt, naCols=TRUE)
#' one_hot(dt, dropCols=FALSE)
#' one_hot(dt, dropUnusedLevels=TRUE)
#' 
#' @export
#' @import data.table

one_hot <- function(dt, cols="auto", sparsifyNAs=FALSE, naCols=FALSE, dropCols=TRUE, dropUnusedLevels=FALSE){
  # One-Hot-Encode unordered factors in a data.table
  # If cols = "auto", each unordered factor column in dt will be encoded. (Or specifcy a vector of column names to encode)
  # If dropCols=TRUE, the original factor columns are dropped
  # If dropUnusedLevels = TRUE, unused factor levels are dropped
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  OHEID <- NULL
  
  #--------------------------------------------------
  
  # Automatically get the unordered factor columns
  if(cols[1] == "auto") cols <- colnames(dt)[which(sapply(dt, function(x) is.factor(x) & !is.ordered(x)))]
  
  # If there are no columns to encode, return dt
  if(length(cols) == 0) return(dt)
  
  # Build tempDT containing and ID column and 'cols' columns
  tempDT <- dt[, cols, with=FALSE]
  tempDT[, OHEID := .I]
  for(col in cols) set(tempDT, j=col, value=factor(paste(col, tempDT[[col]], sep="_"), levels=paste(col, levels(tempDT[[col]]), sep="_")))
  
  # One-hot-encode
  melted <- melt(tempDT, id = 'OHEID', value.factor = T, na.rm=TRUE)
  if(dropUnusedLevels == TRUE){
    newCols <- dcast(melted, OHEID ~ value, drop = T, fun.aggregate = length)
  } else{
    newCols <- dcast(melted, OHEID ~ value, drop = F, fun.aggregate = length)
  }
  
  # Fill in potentially missing rows
  newCols <- newCols[tempDT[, list(OHEID)]]
  newCols[is.na(newCols[[2]]), setdiff(paste(colnames(newCols)), "OHEID") := 0L]
  
  #--------------------------------------------------
  # Deal with NAs
  
  if(!sparsifyNAs | naCols){
    
    # Determine which columns have NAs
    na_cols <- character(0)
    for(col in cols) if(any(is.na(tempDT[[col]]))) na_cols <- c(na_cols, col)
    
    # If sparsifyNAs is TRUE, find location of NAs in dt and insert them in newCols
    if(!sparsifyNAs)
      for(col in na_cols) newCols[is.na(tempDT[[col]]), intersect(levels(tempDT[[col]]), colnames(newCols)) := NA_integer_]
    
    # If naCols is TRUE, build a vector for each column with an NA value and 1s indicating the location of NAs
    if(naCols)
      for(col in na_cols) newCols[, eval(paste0(col, "_NA")) := is.na(tempDT[[col]]) * 1L]
  }
  
  #--------------------------------------------------
  # Clean Up
  
  # Combine binarized columns with the original dataset
  result <- cbind(dt, newCols[, !"OHEID"])
  
  # Reorder columns
  possible_colnames <- character(0)
  for(col in colnames(dt)){
    possible_colnames <- c(possible_colnames, col)
    if(col %in% cols){
      possible_colnames <- c(possible_colnames, paste0(col, "_NA"))
      possible_colnames <- c(possible_colnames, paste(levels(tempDT[[col]])))
    }
  }
  sorted_colnames <- intersect(possible_colnames, colnames(result))
  setcolorder(result, sorted_colnames)
  
  # If dropCols = TRUE, remove the original factor columns
  if(dropCols == TRUE) result <- result[, !cols, with=FALSE]
  
  return(result)
}
