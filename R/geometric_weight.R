#' @title
#' Geometric Weight
#'
#' @description
#' Generate geometric weights
#'
#' @details
#' Returns a weight based on the formula r^k/sum(r^seq_len(n)). The sequence of weights for k=1, 2, ..., n sum to 1
#'
#' @param k r^k/sum(r^(1, 2, ... n))
#' @param n r^k/sum(r^(1, 2, ... n))
#' @param r r^k/sum(r^(1, 2, ... n))
#'
#' @examples
#' geometric_weight(1:3, n=3, r=1)
#' geometric_weight(1:3, n=3, r=.5)
#' geometric_weight(1:3, n=3, r=2)
#'
#' @export

geometric_weight <- function(k, n, r=1){
  # Returns a set of weights using a geometric sequence

  return(r^k/sum(r^seq_len(n)))
}
