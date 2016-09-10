#' Exponential Weight
#'
#' @export

exponential_weight <- function(k, base=exp(1), offset=0, slope=.1){
  # Returns a weight using exponential decay

  1-base^(offset-slope*k)
}
