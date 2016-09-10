#' Geometric Weight
#'
#' @export

geometric_weight <- function(n=12, r=1.1){
  # Returns a set of weights using a geometric sequence

  return(r^seq_len(n)/sum(r^seq_len(n)))
}
