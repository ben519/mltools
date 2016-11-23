#' @title
#' Gini Impurities
#'
#' @description
#' Identify group weighted gini impurities using pairs of columns within a dataset. Can be used to located hierarchical data, or 1-1 correspondences
#'
#' @details
#' For pairs of columns (Var1, Var2) in a dataset, calculates the weighted gini impurity of Var2 relative to the groups determined by Var1
#' 
#' @param dt A data.table with at least two columns
#' @param wide Should the results be in wide format?
#' @param verbose Should progress be printed to the screen?
#'
#' @export
#' @import data.table
#' @importFrom stats weighted.mean
#'
#' @examples
#' library(data.table)
#' gini_impurities(alien.train)
#' gini_impurities(alien.train, wide=TRUE)

gini_impurities <- function(dt, wide=FALSE, verbose=FALSE){
  # Returns pairs of categorical fields (cat1, cat2, GiniImpurity) where GiniImpurity is the weighted gini impurity of
  # cat2 relative to the groups determined by cat1
  # NA values are treated as if they were another factor level
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  Var1 <- NULL
  Var2 <- NULL
  GiniImpurity <- NULL
  N <- NULL
  value <- NULL
  Samples <- NULL
  variable <- NULL
  i.GiniImpurity <- NULL

  #--------------------------------------------------
  # Subset dt by just the categorical fields

  catfields <- colnames(dt)[sapply(dt, function(x) is.factor(x) | is.character(x) | is.logical(x))]
  cats1 <- dt[, catfields, with=FALSE]

  # Build a table to store the results
  varpairs <- CJ(Var1=catfields, Var2=catfields, sorted=FALSE)
  varpairs[Var1==Var2, GiniImpurity := 0]

  # Loop through each grouping variable
  for(catcol in catfields){
    if(verbose) print(paste("Calculating gini impurities by field:", catcol))

    setkeyv(cats1, catcol)
    impuritiesDT <- cats1[, list(Samples=.N), keyby=catcol]

    # Looop through each of the other categorical columns
    for(colname in setdiff(catfields, catcol)){
      if(verbose) print(paste("Calculating gini impurity of field:", colname, " grouped by field:", catcol))

      # Get the gini impurity for each pair (catcol, other)
      counts <- cats1[, list(.N), by=c(catcol, colname)]
      impurities <- counts[, list(GiniImpurity=sum((N/sum(N))*(1-N/sum(N)))), by=catcol]
      impuritiesDT[impurities, GiniImpurity := GiniImpurity]
      setnames(impuritiesDT, "GiniImpurity", colname)
    }

    cats1.gini <- melt(impuritiesDT, id.vars=c(catcol, "Samples"))
    cats1.gini <- cats1.gini[, list(GiniImpurity=weighted.mean(x=value, w=Samples)), by=variable]
    cats1.gini <- cats1.gini[, list(Var1=catcol, Var2=variable, GiniImpurity)]
    varpairs[cats1.gini, `:=`(GiniImpurity=i.GiniImpurity), on=c("Var1", "Var2")]
  }

  # Check if the user wants the result to be in wide format
  if(wide) varpairs <- dcast(varpairs, Var1 ~ Var2, value.var="GiniImpurity")

  return(varpairs[])
}
