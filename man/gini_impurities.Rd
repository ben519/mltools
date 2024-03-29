% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gini_impurities.R
\name{gini_impurities}
\alias{gini_impurities}
\title{Gini Impurities}
\usage{
gini_impurities(dt, wide = FALSE, verbose = FALSE)
}
\arguments{
\item{dt}{A data.table with at least two columns}

\item{wide}{Should the results be in wide format?}

\item{verbose}{Should progress be printed to the screen?}
}
\description{
Identify group weighted gini impurities using pairs of columns within a dataset. Can be used to located hierarchical data, or 1-1 correspondences
}
\details{
For pairs of columns (Var1, Var2) in a dataset, calculates the weighted gini impurity of Var2 relative to the groups determined by Var1
}
\examples{
library(data.table)
gini_impurities(alientrain)
gini_impurities(alientrain, wide=TRUE)
}
