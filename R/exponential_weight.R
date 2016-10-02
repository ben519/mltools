#' @title
#' Exponential Weight
#'
#' @description
#' Generate exponential weights
#'
#' @details
#' Returns a weight based on the formula 1-base^(offset-slope*k)
#'
#' @param k
#' @param base
#' @param offset
#' @param slope
#'
#' @examples
#' exponential_weight(1:3, slope=.1)
#' exponential_weight(1:3, slope=1)
#' exponential_weight(1:3, slope=10)
#'
#' @export

exponential_weight <- function(k, base=exp(1), offset=0, slope=.1){
  # Returns a weight using exponential decay

  1-base^(offset-slope*k)
}
