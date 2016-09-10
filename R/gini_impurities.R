#' Gini Impurities
#'
#' @export
#' @import data.table

gini_impurities <- function(dt, verbose=FALSE){
  # Returns pairs of categorical fields (cat1, cat2, GI) where GI is the weighted gini impurity of
  # cat2 relative to the groups determined by cat1
  # NA values are treated as if they were another factor level

  #--------------------------------------------------
  # Subset dt by just the categorical fields

  catfields <- colnames(dt)[sapply(dt, function(x) is.factor(x) | is.logical(x))]
  cats1 <- dt[, catfields, with=FALSE]

  # Build a table to store the results
  varpairs <- CJ(Var1=catfields, Var2=catfields, sorted=FALSE)
  varpairs[Var1==Var2, GI := 0]

  # Loop through each grouping variable
  for(catcol in catfields){
    if(verbose) print(paste("Calculating gini impurities by field:", catcol))

    setkeyv(cats1, catcol)
    impuritiesDT <- cats1[, list(Samples=.N), keyby=catcol]

    # Looop through each of the other categorical columns
    for(colname in setdiff(catfields, catcol)){

      # Get the gini impurity for each pair (catcol, other)
      counts <- cats1[, list(.N), by=c(catcol, colname)]
      impurities <- counts[, list(GI=sum((N/sum(N))*(1-N/sum(N)))), by=catcol]
      impuritiesDT[impurities, GI := GI]
      setnames(impuritiesDT, "GI", colname)
    }

    cats1.gini <- melt(impuritiesDT, id.vars=c(catcol, "Samples"))
    cats1.gini <- cats1.gini[, list(GI=weighted.mean(x=value, w=Samples)), by=variable]
    cats1.gini <- cats1.gini[, list(Var1=catcol, Var2=variable, GI)]
    varpairs[cats1.gini, `:=`(GI=i.GI), on=c("Var1", "Var2")]
  }

  return(varpairs[])
}
