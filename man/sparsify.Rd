% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparsify.R
\name{sparsify}
\alias{sparsify}
\title{Sparsify}
\usage{
sparsify(
  dt,
  sparsifyNAs = FALSE,
  naCols = "none",
  sparsifyCols = NULL,
  memEfficient = FALSE
)
}
\arguments{
\item{dt}{A data.table object}

\item{sparsifyNAs}{Should NAs be converted to 0s and sparsified?}

\item{naCols}{\itemize{
 \item{\bold{"none"}} Don't generate columns to identify NA values
 \item{\bold{"identify"}} For each column of dt with an NA value, generate a column in the 
 sparse matrix with 1s indicating NAs. Columns will be named like "color_NA"
 \item{\bold{"efficient"}} For each column of dt with an NA value, generate a column in the 
 sparse matrix with 1s indicating either NAs or Non NAs - whichever is more memory efficient. 
 Columns will be named like "color_NA" or "color_NotNA"
}}

\item{sparsifyCols}{What columns to use. Use this to exclude columns of dt from being sparsified without having 
to build a column-subsetted copy of dt to input into sparsify(...). Default = NULL means use
all columns of dt.}

\item{memEfficient}{Default = FALSE. Set this to TRUE for a slower but more memory efficient process}
}
\description{
Convert a data.table object into a sparse matrix (with the same number of rows).
}
\details{
Converts a data.table object to a sparse matrix (class "dgCMatrix"). Requires the \pkg{Matrix} package. 
All sparsified data is assumed to take on the value 0/FALSE 

### Data Type | Description & NA handling

numeric | If \code{sparsifyNAs} = FALSE, only 0s will be sparsified
          If \code{sparsifyNAs} = TRUE, 0s and NAs will be sparsified

factor (unordered) | Each level will generate a sparsified binary column
                     Column names are feature_level, e.g. {"color_red", "color_blue"}

factor (ordered) | Levels are converted to numeric, 1 - NLevels
                   If \code{sparsifyNAs} = FALSE, NAs will remain as NAs
                   If \code{sparsifyNAs} = TRUE, NAs will be sparsified

logical | TRUE and FALSE values will be converted to 1s and 0s
          If \code{sparsifyNAs} = FALSE, only FALSEs will be sparsified
          If \code{sparsifyNAs} = TRUE, FALSEs and NAs will be sparsified
}
\examples{
library(data.table)
library(Matrix)

dt <- data.table(
  intCol=c(1L, NA_integer_, 3L, 0L),
  realCol=c(NA, 2, NA, NA),
  logCol=c(TRUE, FALSE, TRUE, FALSE),
  ofCol=factor(c("a", "b", NA, "b"), levels=c("a", "b", "c"), ordered=TRUE),
  ufCol=factor(c("a", NA, "c", "b"), ordered=FALSE)
)

sparsify(dt)
sparsify(dt, sparsifyNAs=TRUE)
sparsify(dt[, list(realCol)], naCols="identify")
sparsify(dt[, list(realCol)], naCols="efficient")
}
