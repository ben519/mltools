#' @title
#' Get Folds
#'
#' @description
#' Map a target vector to a set of FoldIDs of equal length (or nearly equal length)
#'
#' @details
#' Convenient method for mapping each instance to a FoldID, potentially with stratification
#'
#' @param target A vector of target values
#' @param stratified Should target classes be split proportionally (how to handle numeric target?)
#' @param folds How many folds
#' @param seed Random number seed
#'
#' @examples
#' get_folds(iris$Species, 5L)
#' get_folds(iris$Species, 5L, stratified=TRUE)
#'
#' @export

get_folds <- function(target, folds=5L, stratified=FALSE, seed=NULL){
  # Assign each element of target to a fold
  # If stratified=TRUE, resulting map will ensure that an equal number (possibly off by 1) of each class in target gets mapped to each fold
  
  set.seed(seed)
  
  if(stratified){
    dt <- data.table(Target=target, Idx=seq_len(length(target)))
    dt[, FoldID := (sample(.N, .N) - 1L) %% folds + 1L, by=Target]
    foldIDs <- dt$FoldID
  } else{
    foldIDs <- (sample(length(target), length(target)) - 1L) %% folds + 1L
  }
  
  return(foldIDs)
}
