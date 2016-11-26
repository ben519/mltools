#' @title
#' Sparsify
#'
#' @description
#' Convert a data.table object into a sparse matrix (with the same number of rows).
#'
#' @details
#' Converts a data.table object to a sparse matrix (class "dgCMatrix"). Requires the \pkg{Matrix} package. 
#' All sparsified data is assumed to take on the value 0/FALSE 
#' 
#' ### Data Type | Description & NA handling
#' 
#' numeric | If \code{sparsifyNAs} = FALSE, only 0s will be sparsified
#'           If \code{sparsifyNAs} = TRUE, 0s and NAs will be sparsified
#'
#' factor (unordered) | Each level will generate a sparsified binary column
#'                      Column names are feature_level, e.g. {"color_red", "color_blue"}
#'
#' factor (ordered) | Levels are converted to numeric, 1 - NLevels
#'                    If \code{sparsifyNAs} = FALSE, NAs will remain as NAs
#'                    If \code{sparsifyNAs} = TRUE, NAs will be sparsified
#'
#' logical | TRUE and FALSE values will be converted to 1s and 0s
#'           If \code{sparsifyNAs} = FALSE, only FALSEs will be sparsified
#'           If \code{sparsifyNAs} = TRUE, FALSEs and NAs will be sparsified
#' 
#' @param dt A data.table object
#' @param sparsifyNAs Should NAs be converted to 0s and sparsified?
#' @param naCols
#' \itemize{
##'  \item{\bold{"none"}} Don't generate columns to identify NA values
##'  \item{\bold{"identify"}} For each column of dt with an NA value, generate a column in the 
##'  sparse matrix with 1s indicating NAs. Columns will be named like "color_NA"
##'  \item{\bold{"efficient"}} For each column of dt with an NA value, generate a column in the 
##'  sparse matrix with 1s indicating either NAs or Non NAs - whichever is more memory efficient. 
##'  Columns will be named like "color_NA" or "color_NotNA"
##' }
#'
#' @export
#' @import data.table
#' @import Matrix
#'
#' @examples
#' library(data.table)
#' library(Matrix)
#' 
#' dt <- data.table(
#'   intCol=c(1L, NA_integer_, 3L, 0L),
#'   realCol=c(NA, 2, NA, NA),
#'   logCol=c(TRUE, FALSE, TRUE, FALSE),
#'   ofCol=factor(c("a", "b", NA, "b"), levels=c("a", "b", "c"), ordered=TRUE),
#'   ufCol=factor(c("a", NA, "c", "b"), ordered=FALSE)
#' )
#' 
#' sparsify(dt)
#' sparsify(dt, sparsifyNAs=TRUE)
#' sparsify(dt[, list(realCol)], naCols="identify")
#' sparsify(dt[, list(realCol)], naCols="efficient")

sparsify <- function(dt, sparsifyNAs=FALSE, naCols="none"){
  # Convert a data.table object to a sparse matrix (class "dgCMatrix")
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  NACol <- NULL
  ColumnIdx <- NULL
  NewColumn <- NULL
  Column <- NULL
  PcntNA <- NULL
  Type <- NULL
  ColIdx <- NULL
  SparseRowIdx <- NULL
  Val <- NULL
  variable <- NULL
  
  #--------------------------------------------------
  
  # Check inputs
  if(! naCols %in% c("none", "identify", "efficient")) 
    stop("Argument 'naCols' not recognized. Should be one of {\"none\", \"identify\", \"efficient\"}")
  
  # Helper method
  data_type <- function(vec){
    if(is.integer(vec)) return("integer")
    if(is.double(vec)) return("double")
    else if(is(vec, "logical")) return("logical")
    else if(is(vec, "character")) return("character")
    else if(is(vec, "factor")){
      if(is.ordered(vec)) return("ordered factor") else return("unordered factor")
    } else return("other")
  }
  
  # Get info about the columns
  cols <- data.table(ColumnIdx = seq_len(ncol(dt)), Column=colnames(dt))
  for(col in cols$Column) cols[Column == col, `:=`(PcntNA = sum(is.na(dt[[col]]))/nrow(dt), Type = data_type(dt[[col]]))][]
  
  # Build list to store sparse matrices as they get built
  matList <- list()
  
  # Build vectors to store new column info
  matcols.na <- character(0)
  matcols.numeric <- character(0)
  matcols.logical <- character(0)
  matcols.ofactor <- character(0)
  matcols.ufactor <- character(0)
  
  # Build sparse matrix that identifies NA values
  if(naCols == "identify"){
    
    # Cols with pcnt 0 < NA
    na_cols <- cols[PcntNA > 0]$Column
    if(length(na_cols) > 0){
      sparse.NAs <- Matrix(is.na(as.matrix(dt[, na_cols, with=FALSE]))*1L, sparse = TRUE)
      colnames(sparse.NAs) <- paste0(na_cols, "_NA")
      matList <- c(matList, list(sparse.NAs))
      matcols.na <- colnames(sparse.NAs); names(matcols.na) <- na_cols
    }
    
  } else if(naCols == "efficient"){
    
    # Cols with 0 < NA <= 0.5
    na_cols <- cols[PcntNA > 0 & PcntNA <= 0.5]$Column
    if(length(na_cols) > 0){
      sparse.NAs <- Matrix(is.na(as.matrix(dt[, na_cols, with=FALSE]))*1L, sparse = TRUE)
      colnames(sparse.NAs) <- paste0(na_cols, "_NA")
      matList <- c(matList, list(sparse.NAs))
      matcols.na <- colnames(sparse.NAs); names(matcols.na) <- na_cols
    }
    
    # Cols with 0.5 < NA
    not_na_cols <-  cols[PcntNA > 0.5]$Column
    if(length(not_na_cols) > 0){
      sparse.NotNAs <- Matrix(!is.na(as.matrix(dt[, not_na_cols, with=FALSE]))*1L, sparse = TRUE)
      colnames(sparse.NotNAs) <- paste0(not_na_cols, "_NotNA")
      matList <- c(matList, list(sparse.NotNAs))
      newcols <- colnames(sparse.NotNAs); names(newcols) <- not_na_cols
      matcols.na <- c(matcols.na, newcols)
    }
  }
  
  # Build sparse matrix from numeric features
  cols.numeric <- cols[Type %in% c("integer", "double")]$Column
  if(length(cols.numeric) > 0){
    sparse.numerics <- dt[, cols.numeric, with=FALSE]
    if(sparsifyNAs) for(col in cols.numeric) set(sparse.numerics, j=col, value=replace_na(sparse.numerics[[col]], 0L))
    sparse.numerics <- Matrix(as.matrix(sparse.numerics), sparse=TRUE)
    matList <- c(matList, list(sparse.numerics))
    matcols.numeric <- colnames(sparse.numerics); names(matcols.numeric) <- cols.numeric
  }
  
  # Build sparse matrix from logical features
  cols.logical <- cols[Type %in% c("logical")]$Column
  if(length(cols.logical) > 0){
    sparse.logicals <- dt[, cols.logical, with=FALSE]
    if(sparsifyNAs) for(col in cols.logical) set(sparse.logicals, j=col, value=replace_na(sparse.logicals[[col]], FALSE))
    sparse.logicals <- Matrix(as.matrix(sparse.logicals), sparse=TRUE)
    matList <- c(matList, list(sparse.logicals))
    matcols.logical <- colnames(sparse.logicals); names(matcols.logical) <- cols.logical
  }
  
  # Build sparse matrix from ordered factor features
  cols.ofactor <- cols[Type %in% c("ordered factor")]$Column
  if(length(cols.ofactor) > 0){
    sparse.ofactors <- dt[, cols.ofactor, with=FALSE]
    for(col in cols.ofactor) set(sparse.ofactors, j=col, value=as.numeric(sparse.ofactors[[col]]))
    if(sparsifyNAs) for(col in cols.ofactor) set(sparse.ofactors, j=col, value=replace_na(sparse.ofactors[[col]], 0L))
    sparse.ofactors <- Matrix(as.matrix(sparse.ofactors), sparse=TRUE)
    matList <- c(matList, list(sparse.ofactors))
    matcols.ofactor <- colnames(sparse.ofactors); names(matcols.ofactor) <- cols.ofactor
  }
  
  # Build sparse matrix from unordered factor features
  cols.ufactor <- cols[Type %in% c("unordered factor")]$Column
  if(length(cols.ufactor) > 0){
    factor_levels <- data.table(variable=character(0), level=character(0))
    for(col in cols.ufactor) factor_levels <- rbind(factor_levels, data.table(variable=col, level=levels(dt[[col]])))
    factor_levels[, ColIdx := .I]
    suppressWarnings(dt[, SparseRowIdx := .I]) # insert SparseRowIdx and suppress warnings about modifying the table
    factor_vals <- melt(dt, id.vars="SparseRowIdx", measure.vars=cols.ufactor, variable.factor=FALSE, value.name="level", na.rm = TRUE)
    factor_vals <- factor_vals[factor_levels, on=c("variable", "level"), nomatch=0]
    factor_vals[, Val := 1L]
    if(!sparsifyNAs){
      for(col in cols.ufactor){
        naDT <- dt[is.na(get(col)), list(SparseRowIdx, variable=col, Val=NA_integer_)]
        if(nrow(naDT) > 0){
          naDT <- merge(naDT, factor_levels[variable==col], all=TRUE)
          factor_vals <- rbind(factor_vals, naDT, use.names=TRUE)
        }
      }
    }
    dt[, SparseRowIdx := NULL]  # remove SparseRowIdx
    sparse.ufactors <- sparseMatrix(
      i = factor_vals$SparseRowIdx,
      j = factor_vals$ColIdx,
      x = factor_vals$Val,
      dims = c(nrow(dt), nrow(factor_levels)),
      dimnames = list(NULL, paste(factor_levels$variable, factor_levels$level, sep="_"))
    )
    matList <- c(matList, list(sparse.ufactors))
    matcols.ufactor <- paste(factor_levels$variable, factor_levels$level, sep="_"); names(matcols.ufactor) <- factor_levels$variable
  }
  
  # Combine sparse matrices
  sparse.all <- do.call(cbind, matList)
  
  # Fix the column order (order columns in same order they were given, with NA columns leading each column group)
  oldcols <- c(names(matcols.na), names(matcols.numeric), names(matcols.logical), names(matcols.ofactor), names(matcols.ufactor))
  newcols <- c(matcols.na, matcols.numeric, matcols.logical, matcols.ofactor, matcols.ufactor)
  col.names <- data.table(OldColumn=oldcols, NewColumn=newcols)
  col.names[, NACol := FALSE]
  col.names[seq_along(matcols.na), NACol := TRUE]
  col.names[cols, ColumnIdx := ColumnIdx, on=c("OldColumn"="Column")]
  col.names <- col.names[order(ColumnIdx, -NACol, NewColumn)]
  sparse.all <- sparse.all[, col.names$NewColumn]
  
  return(sparse.all)
}
