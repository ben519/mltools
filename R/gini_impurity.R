#' Gini Impurity
#'
#' @export

gini_impurity <- function(vals){
  # Returns the gini impurity of a set of categorical values
  # vals can either be the raw category instances (vals=c("red", "red", "blue", "green")) or named category frequencies (vals=c(red=2, blue=1, green=1))
  # Gini Impurity is the probability a value is incorrectly labeled when labeled according to the distribution of classes in the set

  if(is(vals, "numeric")) counts <- vals else counts <- table(vals)
  total <- sum(counts)

  return(sum((counts/total)*(1-counts/total)))
}
