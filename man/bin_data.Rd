% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bin_data.R
\name{bin_data}
\alias{bin_data}
\title{Map a vector of numeric values into bins}
\usage{
bin_data(
  x = NULL,
  binCol = NULL,
  bins = 10,
  binType = "explicit",
  boundaryType = "lcro]",
  returnDT = FALSE,
  roundbins = FALSE
)
}
\arguments{
\item{x}{A vector of values or a data.table object}

\item{binCol}{A column of \code{dt} specifying the values to bin}

\item{bins}{\itemize{
 \item{\bold{integer}} {specifying the number of bins to generate}
 \item{\bold{numeric vector}} {specifying sequential bin boundaries \{(x0, x1), (x1, x2), ..., (xn-1, xn)\}}
 \item{\bold{2-column data.frame/data.table}} {each row defines a bin}
}}

\item{binType}{\itemize{
 \item{\bold{"explicit"}} {interpret bins as they are given}
 \item{\bold{"quantile"}} {interpret bins as quantiles (empty quantile bins will be discarded)}
}}

\item{boundaryType}{\itemize{
 \item{\bold{"lcro]"}} {bins are [left-closed, right-open) except for last bin which is [left-closed, right-closed]}
 \item{\bold{"lcro)"}} {bins are [left-closed, right-open)}
 \item{\bold{"[lorc"}} {bins are (left-open, right-closed] except for first bin which is [left-closed, right-closed]}
 \item{\bold{"(lorc"}} {bins are (left-open, right-closed]}
}}

\item{returnDT}{If \bold{FALSE}, return an ordered factor of bins corresponding to the values given, else return
a data.table object which includes all bins and values (makes a copy of data.table object if given)}

\item{roundbins}{Should bin values be rounded? (Only applicable for binType = "quantile")
\itemize{
 \item{\bold{FALSE}} {bin values are not rounded}
 \item{\bold{TRUE}} {NOT YET IMPLEMENTED. bin values are rounded to the lowest decimal such that data-to-bin mapping is not altered}
 \item{\bold{non-negative integer}} {bin values are rounded to this many decimal places}
}}
}
\description{
Takes a vector of values and bin parameters and maps each value to an ordered factor whose levels are a set of bins like [0,1), [1,2), [2,3).

Values may be provided as a vector or via a pair of parameters - a data.table object and the name of the column to bin.
}
\details{
This function can return two different types of output, depending on whether \code{returnDT} is \code{TRUE} or \code{FALSE}.

If \code{returnDT=FALSE}, returns an ordered factor vector of bins like [1, 2), [-3,-2), ... corresponding to the values which were
binned and whose levels correspond to all the generated bins. (Note that empty bins may be present as unused factor levels).

If \code{returnDT=TRUE}, returns a data.table object with all values and all bins (including empty bins). If \code{dt} is provided
instead of \code{vals}, a full copy of \code{dt} is created and merged with the set of generated bins.
}
\examples{
library(data.table)
iris.dt <- data.table(iris)

# custom bins
bin_data(iris.dt, binCol="Sepal.Length", bins=c(4, 5, 6, 7, 8))

# 10 equally spaced bins
bin_data(iris$Petal.Length, bins=10, returnDT=TRUE)

# make the last bin [left-closed, right-open)
bin_data(c(0,0,1,2), bins=2, boundaryType="lcro)", returnDT=TRUE)

# bin values by quantile
bin_data(c(0,0,0,0,1,2,3,4), bins=4, binType="quantile", returnDT=TRUE)

}
