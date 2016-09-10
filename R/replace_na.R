#' Replace NA values
#'
#' @export

replace_na <- function (x, repl="auto") {
  # Helper method to convert NAs in a vector to 0

  if(as.character(repl) == "auto"){
    if(is(x, "integer")) repl=0L
    else if(is(x, "numeric")) repl=0
    else if(is(x, "character")) repl=""
    else repl <- NA
  }

  x[is.na(x)] <- repl
  return(x)
}
