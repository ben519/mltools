#' Explore a dataset
#'
#' @export
#' @import data.table

explore_dataset <- function(dt1, dt2=NULL, targetCol=NULL, verbose=FALSE){
  # Analyze the given dataset(s)

  # How many rows and columns
  print(paste("Data has", nrow(dt1), "rows and", ncol(dt1), "columns"))

  # Cardinality. Any fields with 1 value or all unique values?
  cardinalities <- dt1[, lapply(.SD, uniqueN)]
  for(colname in colnames(cardinalities)) if (cardinalities[[colname]] == 1) print(paste(colname, "only has 1 value"))
  for(colname in colnames(cardinalities)) if (cardinalities[[colname]] == nrow(dt1)) print(paste(colname, "has all unique values"))

  #--------------------------------------------------
  # Gini Impurity Matrix

  if(verbose) print("Building gini impurity matrix")

  # Notes:
  # if (i, j) is 0, then you can guess the value of j by knowing i
  # if (i,j) = (j,i) = 0 then fields i and j are copies of each other
  # if (i,j) = 0 and (j, i) > 0 then field i is (likely) a child of field j (i.e. a hierarchy structure is present)

  varpairs <- giniImpurities(dt1, verbose=verbose)
  giniMatrix <- dcast(varpairs, Var1 ~ Var2, value.var="GI")

  #--------------------------------------------------
  # Field skewness

  fieldSkewness <- skewness(dt1)

  return(list(cardinalities, giniMatrix, fieldSkewness))
}

giniImpurities <- function(dt, verbose=FALSE){
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
