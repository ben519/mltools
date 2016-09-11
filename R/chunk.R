#' Chunk
#'
#' @export

chunk <- function(dt, chunks){
  split(dt, cut(seq_len(nrow(dt)), chunks, labels = FALSE))
}
