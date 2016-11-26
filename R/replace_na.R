#' @title
#' Replace NA Values
#' 
#' @description
#' Convience method for returning a copy of a vector such that NA values are substituted with a replacement value
#' 
#' @details
#' Returns a copy of \code{x} such that NAs get replaced with a replacement value.  Default replacement value is 0.
#' 
#' @param x vector of values
#' @param repl what to substitute in place of NAs
#'
#' @examples
#' replace_na(c(1, NA, 1, 0))
#' 
#' @export

replace_na <- function(x, repl="auto") {
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
