#' Chunk
#'
#' @export

chunk <- function(x, chunks){
  split(x, cut(seq_along(x), chunks, labels = FALSE))
}
