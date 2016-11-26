#' @title
#' Cross Validation Folds
#'
#' @description
#' Map an object \code{x} into equal (or nearly equal) size folds.
#' If \code{x} is a vector, a matching vector of FoldIDs is returned.
#' If \code{x} is a data.table, a list of partitions of x is returned.
#'
#' @details
#' Convenient method for mapping an object into equal size folds, potentially with stratification
#'
#' @param x A vector of values or a data.table object
#' @param stratified If x is a vector then TRUE or FALSE indicating whether x's split the class's of x proportionally. If x
#' is a data.table then \code{stratified} should be FALSE or the name of a column in x on which to perform stratification.
#' @param nfolds How many folds?
#' @param seed Random number seed
#'
#' @examples
#' folds(alien.train$IsAlien, nfolds=2)
#' folds(alien.train$IsAlien, nfolds=2, stratified=TRUE, seed=2016)
#' folds(alien.train, nfolds=2, stratified="IsAlien", seed=2016)
#'
#' @export
#' @import data.table

folds <- function(x, nfolds=5L, stratified=FALSE, seed=NULL){
  # Assign each element of x to a fold
  # If stratified=TRUE, resulting map will ensure that an equal number (possibly off by 1) of each class in x gets mapped to each fold
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  FoldID <- NULL
  Target <- NULL
  
  #--------------------------------------------------
  # If x is a data.table
  
  if(is(x, "data.table")){
    if(stratified != FALSE){
      if(!stratified %in% colnames(x)) stop("Argument stratified must be FALSE or match the name of a column in the x data.table")
      foldIDs <- folds(x=x[[stratified]], nfolds=nfolds, stratified=TRUE, seed=seed)  # recursive call
    } else{
      foldIDs <- folds(x=seq_len(nrow(x)), nfolds=nfolds, stratified=FALSE, seed=seed)  # recursive call
    }
    return(split(x, foldIDs))
  }
  
  #--------------------------------------------------
  # If x is a vector
  
  set.seed(seed)
  if(stratified){
    dt <- data.table(Target=x, Idx=seq_len(length(x)))
    dt[, FoldID := (sample(.N, .N) - 1L) %% nfolds + 1L, by=Target]
    foldIDs <- dt$FoldID
  } else{
    foldIDs <- (sample(length(x), length(x)) - 1L) %% nfolds + 1L
  }
  
  return(foldIDs)
}
