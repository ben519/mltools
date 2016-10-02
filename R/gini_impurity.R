#' @title
#' Gini Impurity
#'
#' @description
#' Calculates the Gini Impurity of a set. Gini Impurity is a measure of how often a randomly chosen element from the set would be
#' incorrectly labeled if it was randomly labeled according to the distribution of labels in the set. Values may be provided as a
#' vector or via a pair of parameters - a data.table object and the name of the column to bin.
#'
#' @details
#' Returns a real value in the range [0, 1]
#'
#' @param vals A vector of values. Values can be given as raw instances like c("red", "red", "blue", "green") or as a named vector
#' of class frequencies like c(red=2, blue=1, green=1)
#'
#' @examples
#' gini_impurity(c("red", "red", "blue", "green"))
#' gini_impurity(c(red=2, blue=1, green=1))
#'
#' @export

gini_impurity <- function(vals){
  # Returns the gini impurity of a set of values
  # vals can either be raw category instances (e.g. c("red", "red", "blue", "green")) or named category frequencies (e.g. c(red=2, blue=1, green=1))
  # Gini Impurity is the probability a value is incorrectly labeled when labeled according to the distribution of classes in the set

  if(is(vals, "numeric")) counts <- vals else counts <- table(vals)
  total <- sum(counts)

  return(sum((counts/total)*(1-counts/total)))
}
