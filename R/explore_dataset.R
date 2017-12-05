#' @title
#' Explore Dataset
#'
#' @description
#' (Experimental) Automated Exploratory Data Analysis
#'
#' @details
#' Expirimental. Evaluates and summarizes the data in every column of a data.table. 
#' Can identify columns with hierarchical structure and columns with perfectly correlated 
#' values.
#' 
#' @param dt1 dataset to analyze
#' @param dt2 (optional) second dataset to analyze, with the same columns as dt1
#' @param targetCol Name of the column you're trying to model/predict
#' @param verbose Should the exploratory process steps be displayed?
#'
#' @export
#' @import data.table
#'
#' @examples
#' library(data.table)
#' explore_dataset(alien.train)

explore_dataset <- function(dt1, dt2=NULL, targetCol=NULL, verbose=FALSE){
  # Analyze the given dataset(s)

  # How many rows and columns
  print(paste("Data has", nrow(dt1), "rows and", ncol(dt1), "columns"))

  # Cardinality. Any fields with 1 value or all unique values?
  cardinalities <- dt1[, lapply(.SD, uniqueN)]
  for(colname in colnames(cardinalities)) if (cardinalities[[colname]] == 1) print(paste(colname, "only has 1 value"))
  for(colname in colnames(cardinalities)) if (cardinalities[[colname]] == nrow(dt1)) print(paste(colname, "has all unique values"))

  #--------------------------------------------------
  # Gini Impurity Matrix

  if(verbose) print("Building gini impurity matrix")

  # Notes:
  # if (i, j) is 0, then you can guess the value of j by knowing i
  # if (i,j) = (j,i) = 0 then fields i and j are copies of each other
  # if (i,j) = 0 and (j, i) > 0 then field i is (likely) a child of field j (i.e. a hierarchy structure is present)

  varpairs <- gini_impurities(dt1, verbose=verbose)
  giniMatrix <- dcast(varpairs, Var1 ~ Var2, value.var="GiniImpurity")

  #--------------------------------------------------
  # Field skewness

  fieldSkewness <- skewness(dt1)

  return(list(cardinalities, giniMatrix, fieldSkewness))
}
