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
#' @param sparsifyCols What columns to use. Use this to exclude columns of dt from being sparsified without having 
#'                     to build a column-subsetted copy of dt to input into sparsify(...). Default = NULL means use
#'                     all columns of dt.
#' @param memEfficient Default = FALSE. Set this to TRUE for a slower but more memory efficient process
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

sparsify <- function(dt, sparsifyNAs = FALSE, naCols = "none", sparsifyCols = NULL, memEfficient = FALSE){
  # Convert a data.table object to a sparse matrix (class "dgCMatrix")
  # If 'sparsifyCols' is NULL, a columns of dt will be sparsified, otherwise only
  # the specified columns will be sparsified
  # If 'memEfficient' is TRUE, a slower but more memory efficient method is used
  
  #--- Hack --------------------------------------
  # Pass 'no visible binding for global variable' notes from R CMD check
  
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
  RowIdx <- NULL
  
  #--- Check Inputs --------------------------------------
  
  if(!is.data.table(dt))
    stop("dt must be a data.table object")
  
  if(!naCols %in% c("none", "identify", "efficient")) 
    stop("Argument 'naCols' not recognized. Should be one of {\"none\", \"identify\", \"efficient\"}")
  
  if(is.null(sparsifyCols)) sparsifyCols <- colnames(dt)
  if(length(setdiff(sparsifyCols, colnames(dt))) > 0) stop("'sparsifyCols' has at least one value not in 'dt'")
  if(length(intersect(sparsifyCols, colnames(dt))) == 0) stop("You cannot attempt to sparsify 0 columns")
  
  #--- Helper Methods --------------------------------------
  
  data_type <- function(vec){
    if(is.integer(vec)){
      return("integer")
    } else if(is.double(vec)){
      return("double")
    } else if(is(vec, "logical")){
      return("logical")
    } else if(is(vec, "character")){
      return("character")
    } else if(is(vec, "factor")){
      if(is.ordered(vec)) return("ordered factor") else return("unordered factor")
    } else return("other")
  }
  
  #--- Prep --------------------------------------
  
  # Get info about the columns
  cols <- data.table(ColumnIdx = seq_len(ncol(dt)), Column = colnames(dt))
  cols <- cols[Column %in% sparsifyCols]
  for(col in cols$Column){
    cols[Column == col, `:=`(PcntNA = sum(is.na(dt[[col]]))/nrow(dt), Type = data_type(dt[[col]]))][]
  }
  
  # Build list to store sparse matrices as they get built
  matList <- vector(mode = "list", length = 6)
  names(matList) <- c("sparse.NAs", "sparse.NotNAs", "sparse.numerics", "sparse.logicals", "sparse.ofactors",
                      "sparse.ufactors")
  
  # Build vectors to store new column info
  matcols.na <- character(0)
  matcols.numeric <- character(0)
  matcols.logical <- character(0)
  matcols.ofactor <- character(0)
  matcols.ufactor <- character(0)
  
  # Build sparse matrix that identifies NA values
  if(naCols == "identify"){
    
    # Cols with PcntNA > 0
    na_cols <- cols[PcntNA > 0]$Column
    if(length(na_cols) > 0){
      if(memEfficient){
        sparse.NAs <- vector(mode = "list", length = length(na_cols))
        for(col in seq_along(na_cols)){
          temp <- dt[, na_cols[col], with = F][, RowIdx := .I]
          temp <- temp[is.na(temp[[1L]])]
          temp <- sparseMatrix(
            x = 1L, 
            i = temp$RowIdx, 
            j = rep(1L, nrow(temp)), 
            dims = c(nrow(dt), 1L)
          )
          sparse.NAs[[col]] <- temp
        }
        sparse.NAs <- do.call(cbind, sparse.NAs)
      } else{
        sparse.NAs <- Matrix(is.na(as.matrix(dt[, na_cols, with=FALSE]))*1L, sparse = TRUE)
      }
      colnames(sparse.NAs) <- paste0(na_cols, "_NA")
      matList[["sparse.NAs"]] <- sparse.NAs
      matcols.na <- colnames(sparse.NAs)
      names(matcols.na) <- na_cols
      rm(sparse.NAs)
    }
    
  } else if(naCols == "efficient"){
    
    # Cols with 0 < PcntNA <= 0.5
    na_cols <- cols[PcntNA > 0 & PcntNA <= 0.5]$Column
    if(length(na_cols) > 0){
      if(memEfficient){
        sparse.NAs <- vector(mode = "list", length = length(na_cols))
        for(col in seq_along(na_cols)){
          temp <- dt[, na_cols[col], with = F][, RowIdx := .I]
          temp <- temp[is.na(temp[[1L]])]
          temp <- sparseMatrix(
            x = 1L, 
            i = temp$RowIdx, 
            j = rep(1L, nrow(temp)), 
            dims = c(nrow(dt), 1L)
          )
          sparse.NAs[[col]] <- temp
        }
        sparse.NAs <- do.call(cbind, sparse.NAs)
      } else{
        sparse.NAs <- Matrix(is.na(as.matrix(dt[, na_cols, with=FALSE]))*1L, sparse = TRUE)
      }
      colnames(sparse.NAs) <- paste0(na_cols, "_NA")
      matList[["sparse.NAs"]] <- sparse.NAs
      matcols.na <- colnames(sparse.NAs)
      names(matcols.na) <- na_cols
      rm(sparse.NAs)
    }
    
    # Cols with PcntNA > 0.5
    not_na_cols <-  cols[PcntNA > 0.5]$Column
    if(length(not_na_cols) > 0){
      if(memEfficient){
        sparse.NotNAs <- vector(mode = "list", length = length(not_na_cols))
        for(col in seq_along(not_na_cols)){
          temp <- dt[, not_na_cols[col], with = F][, RowIdx := .I]
          temp <- temp[!is.na(temp[[1L]])]
          temp <- sparseMatrix(
            x = 1L, 
            i = temp$RowIdx, 
            j = rep(1L, nrow(temp)), 
            dims = c(nrow(dt), 1L)
          )
          sparse.NotNAs[[col]] <- temp
        }
        sparse.NotNAs <- do.call(cbind, sparse.NotNAs)
      } else{
        sparse.NotNAs <- Matrix(!is.na(as.matrix(dt[, not_na_cols, with=FALSE]))*1L, sparse = TRUE)
      }
      colnames(sparse.NotNAs) <- paste0(not_na_cols, "_NotNA")
      matList[["sparse.NotNAs"]] <- sparse.NotNAs
      newcols <- colnames(sparse.NotNAs)
      names(newcols) <- not_na_cols
      matcols.na <- c(matcols.na, newcols)
      rm(sparse.NotNAs)
    }
  }
  
  # Build sparse matrix from numeric features
  cols.numeric <- cols[Type %in% c("integer", "double")]$Column
  if(length(cols.numeric) > 0){
    if(memEfficient){
      sparse.numerics <- vector(mode = "list", length = length(cols.numeric))
      for(col in seq_along(cols.numeric)){
        temp <- dt[, cols.numeric[col], with = F][, RowIdx := .I]
        temp <- if(sparsifyNAs) temp[temp[[1L]] != 0] else temp[temp[[1L]] != 0 | is.na(temp[[1L]])]
        temp <- sparseMatrix(
          x = temp[[1L]], 
          i = temp$RowIdx, 
          j = rep(1L, nrow(temp)), 
          dims = c(nrow(dt), 1L)
        )
        sparse.numerics[[col]] <- temp
      }
      sparse.numerics <- do.call(cbind, sparse.numerics)
    } else{
      sparse.numerics <- dt[, cols.numeric, with = FALSE]
      if(sparsifyNAs){
        for(col in cols.numeric) set(sparse.numerics, j = col, value = replace_na(sparse.numerics[[col]], 0L))
      }
      sparse.numerics <- as.matrix(sparse.numerics)
      sparse.numerics <- Matrix(sparse.numerics, sparse = TRUE)
    }
    colnames(sparse.numerics) <- cols.numeric
    matList[["sparse.numerics"]] <- sparse.numerics
    matcols.numeric <- colnames(sparse.numerics)
    names(matcols.numeric) <- cols.numeric
    rm(sparse.numerics)
  }
  
  # Build sparse matrix from logical features
  cols.logical <- cols[Type %in% c("logical")]$Column
  if(length(cols.logical) > 0){
    if(memEfficient){
      sparse.logicals <- vector(mode = "list", length = length(cols.logical))
      for(col in seq_along(cols.logical)){
        temp <- dt[, cols.logical[col], with = F][, RowIdx := .I]
        temp <- if(sparsifyNAs) temp[temp[[1L]] != FALSE] else temp[temp[[1L]] != FALSE | is.na(temp[[1L]])]
        temp <- sparseMatrix(
          x = temp[[1L]], 
          i = temp$RowIdx, 
          j = rep(1L, nrow(temp)), 
          dims = c(nrow(dt), 1L)
        )
        sparse.logicals[[col]] <- temp
      }
      sparse.logicals <- do.call(cbind, sparse.logicals)
    } else{
      sparse.logicals <- dt[, cols.logical, with=FALSE]
      if(sparsifyNAs){
        for(col in cols.logical) set(sparse.logicals, j = col, value = replace_na(sparse.logicals[[col]], FALSE))
      }
      sparse.logicals <- Matrix(as.matrix(sparse.logicals), sparse=TRUE)
    }
    colnames(sparse.logicals) <- cols.logical
    matList[["sparse.logicals"]] <- sparse.logicals
    matcols.logical <- colnames(sparse.logicals)
    names(matcols.logical) <- cols.logical
    rm(sparse.logicals)
  }
  
  # Build sparse matrix from ordered factor features
  cols.ofactor <- cols[Type %in% c("ordered factor")]$Column
  if(length(cols.ofactor) > 0){
    if(memEfficient){
      sparse.ofactors <- vector(mode = "list", length = length(cols.ofactor))
      for(col in seq_along(cols.ofactor)){
        temp <- dt[, cols.ofactor[col], with = F][, RowIdx := .I]
        temp[[1L]] <- as.integer(temp[[1L]])
        temp <- if(sparsifyNAs) temp[temp[[1L]] != 0] else temp[temp[[1L]] != 0 | is.na(temp[[1L]])]
        temp <- sparseMatrix(
          x = temp[[1L]], 
          i = temp$RowIdx, 
          j = rep(1L, nrow(temp)), 
          dims = c(nrow(dt), 1L)
        )
        sparse.ofactors[[col]] <- temp
      }
      sparse.ofactors <- do.call(cbind, sparse.ofactors)
    } else{
      sparse.ofactors <- dt[, cols.ofactor, with=FALSE]
      for(col in cols.ofactor) set(sparse.ofactors, j = col, value = as.numeric(sparse.ofactors[[col]]))
      if(sparsifyNAs){
        for(col in cols.ofactor) set(sparse.ofactors, j = col, value = replace_na(sparse.ofactors[[col]], 0L))
      }
      sparse.ofactors <- Matrix(as.matrix(sparse.ofactors), sparse=TRUE)
    }
    colnames(sparse.ofactors) <- cols.ofactor
    matList[["sparse.ofactors"]] <- sparse.ofactors
    matcols.ofactor <- colnames(sparse.ofactors)
    names(matcols.ofactor) <- cols.ofactor
    rm(sparse.ofactors)
  }
  
  # Build sparse matrix from unordered factor features
  cols.ufactor <- cols[Type %in% c("unordered factor")]$Column
  if(length(cols.ufactor) > 0){
    factor_levels <- data.table(variable = character(0), level = character(0))
    for(col in cols.ufactor){
      factor_levels <- rbind(factor_levels, data.table(variable = col, level = levels(dt[[col]])))
    }
    factor_levels[, ColIdx := .I]
    
    # Insert SparseRowIdx and suppress warnings about modifying the table
    suppressWarnings(dt[, SparseRowIdx := .I])
    
    factor_vals <- melt(
      data = dt, 
      id.vars = "SparseRowIdx", 
      measure.vars = cols.ufactor, 
      variable.factor = FALSE, 
      value.name = "level", 
      na.rm = TRUE
    )
    factor_vals <- factor_vals[factor_levels, on=c("variable", "level"), nomatch=0]
    factor_vals[, Val := rep(1L, .N)]  # use rep(1L, .N) in case table has 0 rows
    if(!sparsifyNAs){
      for(col in cols.ufactor){
        naDT <- dt[is.na(get(col)), list(SparseRowIdx, variable = col, Val = NA_integer_)]
        if(nrow(naDT) > 0){
          naDT <- merge(naDT, factor_levels[variable == col], all=TRUE, allow.cartesian = TRUE)
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
    matList[["sparse.ufactors"]] <- sparse.ufactors
    matcols.ufactor <- paste(factor_levels$variable, factor_levels$level, sep="_")
    names(matcols.ufactor) <- factor_levels$variable
    rm(sparse.ufactors)
  }
  
  # Combine sparse matrices
  matList <- Filter(Negate(is.null), matList)
  matList <- do.call(cbind, matList)
  
  # Fix the column order (order columns in same order they were given, with NA columns leading each column group)
  oldcols <- c(
    names(matcols.na), 
    names(matcols.numeric), 
    names(matcols.logical), 
    names(matcols.ofactor), 
    names(matcols.ufactor)
  )
  newcols <- c(
    matcols.na, 
    matcols.numeric, 
    matcols.logical, 
    matcols.ofactor, 
    matcols.ufactor
  )
  col.names <- data.table(OldColumn = oldcols, NewColumn = newcols)
  col.names[, NACol := NewColumn %chin% matcols.na]
  col.names[cols, ColumnIdx := ColumnIdx, on = c("OldColumn" = "Column")]
  col.names <- col.names[order(ColumnIdx, -NACol, NewColumn)]
  matList <- matList[, col.names$NewColumn, drop = FALSE]
  
  return(matList)
}
