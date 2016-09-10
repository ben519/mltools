#' Sparsify
#'
#' @examples
#' dt <- data.table(
#' intCol=c(1L, 2L, 3L, 0L),
#' realCol=c(1, 2, 3, 0),
#' logCol=c(TRUE, FALSE, TRUE, FALSE),
#' ofCol=factor(c("a", "b", NA, "b"), ordered=TRUE),
#' ufCol=factor(c("a", NA, "c", "b"), ordered=FALSE)
#' )
#' sparsify(dt, NAColumn=TRUE)
#'
#' @export
#' @import data.table

sparsify <- function(bg, NAColumn=FALSE, verbose=FALSE){
  # Converts a data.table to a sparse matrix (type dgCMatrix)
  # If NAColumn = TRUE, an extra binary column inidicating NAs will be created for each unordered factor which contains NA values
  # Need to investigate how this works when NAs are present in logical (and other?) columns
  # This might be improvable by converting an unordered factor column with no NAs and 2 levels to a single column instead of 2 columns

  sparseMList <- list()
  bg_cols <- colnames(bg)

  # Loop through the columns of bg
  for(j in seq_along(bg_cols)){
    colname <- bg_cols[j]
    if(verbose) print(colname)

    # Type logical
    if(is(bg[[colname]], "logical")){
      if(sum(is.na(bg[[colname]])) != 0) stop("Missing values deteced in logical field. Please improve bgToSparseMatrix()")
      sparseMList[[j]] <- Matrix(bg[[colname]]*1.0, sparse=TRUE, dimnames=list(NULL, colname))
    }

    # Type numeric
    if(is(bg[[colname]], "numeric")){
      if(sum(is.na(bg[[colname]])) != 0) stop("Missing values deteced in numeric field. Please improve bgToSparseMatrix()")
      sparseMList[[j]] <- Matrix(bg[[colname]]*1.0, sparse=TRUE, dimnames=list(NULL, colname))
    }

    # Type factor
    if(is(bg[[colname]], "factor")){

      # Get the row and column indices
      helperDT <- bg[, colname, with=FALSE]
      helperDT[, `:=`(RowIdx=.I, ColIdx=as.integer(get(colname)))]

      # ordered
      if(is.ordered(bg[[colname]])){
        helperDT <- helperDT[!is.na(ColIdx)]  # remove NAs
        sparseMList[[j]] <- sparseMatrix(i=helperDT$RowIdx, j=rep(1L, nrow(helperDT)), x=as.numeric(helperDT[[colname]]),
                                         dims=c(nrow(bg), 1), dimnames=list(NULL, colname))
      }

      # unordered
      else{

        # Handle NAs
        if(NAColumn & sum(is.na(bg[[colname]])) > 0){
          cols <- paste0(colname, "_", c(levels(bg[[colname]]), "NA"))
          helperDT[is.na(ColIdx), ColIdx := length(cols)]
        } else{
          cols <- paste0(colname, "_", levels(bg[[colname]]))
          helperDT <- helperDT[!is.na(ColIdx)]
        }

        sparseMList[[j]] <- sparseMatrix(i=helperDT$RowIdx, j=helperDT$ColIdx, x=1.0, dims=c(nrow(bg), length(cols)), dimnames=list(NULL, cols))
      }
    }

    # Type other
    if(length(intersect(c("logical", "integer", "numeric", "factor"), class(bg[[colname]]))) == 0){
      print(paste("Column", colname, "type not recognized as one of {logical, numeric, factor}"))
    }
  }

  # cbind the matrices together
  sparseM <- do.call(cbind, sparseMList)
  return(sparseM)
}
