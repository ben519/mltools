#' @title
#' Matthews correlation coefficient
#'
#' @description
#' Calculate Matthews correlation coefficient
#'
#' @details
#' Calculate Matthews correlation coefficient. Provide either
#' 
#' \itemize{
#'  \item{\code{preds} and \code{actuals}} or
#'  \item{\code{TP}, \code{FP}, \code{TN}, and \code{FN}}
#'  \item{\code{confusionM}}
#' }
#' 
#' @param preds A vector of prediction values, or a data.frame or matrix of TRUE/FALSE or 1/0 whose columns correspond to the 
#' possible classes
#' @param actuals A vector of actuals values, or a data.frame or matrix of TRUE/FALSE or 1/0 whose columns correspond to the 
#' possible classes
#' @param TP Count of true positives (correctly predicted 1/TRUE)
#' @param FP Count of false positives (predicted 1/TRUE, but actually 0/FALSE)
#' @param TN Count of true negatives (correctly predicted 0/FALSE)
#' @param FN Count of false negatives (predicted 0/FALSE, but actually 1/TRUE)
#' @param confusionM Confusion matrix whose (i,j) element represents the number of samples with predicted class i and true class j
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Matthews_correlation_coefficient}
#'
#' @examples
#' preds <- c(1,1,1,0,1,1,0,0)
#' actuals <- c(1,1,1,1,0,0,0,0)
#' mcc(preds, actuals)
#' mcc(actuals, actuals)
#' mcc(TP=3, FP=2, TN=2, FN=1)
#' 
#' # Multiclass
#' preds <- data.frame(
#'   setosa = rnorm(n = 150), 
#'   versicolor = rnorm(n = 150), 
#'   virginica = rnorm(n = 150)
#' )
#' preds <- preds == apply(preds, 1, max)
#' actuals <- data.frame(
#'   setosa = rnorm(n = 150), 
#'   versicolor = rnorm(n = 150), 
#'   virginica = rnorm(n = 150)
#' )
#' actuals <- actuals == apply(actuals, 1, max)
#' mcc(preds = preds, actuals = actuals)
#' 
#' # Confusion matrix
#' mcc(confusionM = matrix(c(0,3,3,3,0,3,3,3,0), nrow = 3))
#' mcc(confusionM = matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3))
#' 
#' @export
#' @import data.table

mcc <- function(preds = NULL, actuals = NULL, TP = NULL, FP = NULL, TN = NULL, FN = NULL, confusionM = NULL){
  # Matthews correlation coefficient
  # Either preds and actuals should be given or TP, FP, TN, and FN should be given
  # For the binary class case:
  #   preds and actuals should be vectors with values in {0, 1} or {TRUE, FALSE}
  #   TP, FP, TN, FN should be values representing the count of True Positives, False Positives, True Negatives, False Negatives
  # For the multiclass case:
  #   preds and actuals may be vectors of length N corresponding to the number of samples
  #     if preds and actuals are type factor, the C classes will be inferred from the union of the class levels
  #     if preds and actuals are not type factor, the C classes will be inferred from the union of their unique values
  #   preds and actuals may be dataframes or matrices with N rows and C columns corresponding to the C possible classes
  # Alternatively, a confusion matrix may be provided where the (i,j) element represents the number of samples with 
  #   predicted class i and true class j
  
  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check
  
  Ckk <- NULL
  Ckl <- NULL
  Clm <- NULL
  Cmk <- NULL
  N <- NULL
  NotPredN <- NULL
  NotTruthN <- NULL
  Pred <- NULL
  PredIndex <- NULL
  PredN <- NULL
  RowId <- NULL
  Truth <- NULL
  TruthIndex <- NULL
  TruthN <- NULL
  i.ClassIndex <- NULL
  i.N <- NULL
  i.PredN <- NULL
  i.TruthN <- NULL
  value <- NULL
  
  #--------------------------------------------------
  # Check the inputs
  
  valid_input <- FALSE
  if(!is.null(preds) & !is.null(actuals) & is.null(TP) & is.null(FP) & is.null(TN) & is.null(FN) & is.null(confusionM)) 
    valid_input <- TRUE
  if((!is.null(TP) & !is.null(FP) & !is.null(TN) & !is.null(FN)) & is.null(preds) & is.null(actuals) & is.null(confusionM)) 
    valid_input <- TRUE
  if(!is.null(confusionM) & is.null(preds) & is.null(actuals) & is.null(TP) & is.null(FP) & is.null(TN) & is.null(FN))
    valid_input <- TRUE
  if(!valid_input) stop("Either {'preds' and 'actuals'} or {'TP', 'FP', 'TN', 'FN'} or {'confusionM'} should be provided.")
  
  if(!is.null(preds)){
    if(
      is.data.frame(preds) + is.data.frame(actuals) == 1 | 
      is.matrix(preds) + is.matrix(actuals) == 1 | 
      is.vector(preds) + is.vector(actuals) == 1
    ) stop("'preds' and 'actuals' must be of the same type (vectors, matrices or data.frames)")
  }
  
  #--------------------------------------------------
  # If TP, FP, TN, FN are given, quickly calculate and return mcc value
  
  if(!is.null(TP)){
    # Convert types to double for better precision
    TP <- as.double(TP)
    FP <- as.double(FP)
    TN <- as.double(TN)
    FN <- as.double(FN)
    
    # Calculate MCC
    numerator <- (TP * TN - FP * FN)
    denominator <- sqrt((TP + FP)*(TP + FN)*(TN + FP)*(TN + FN))
    if(denominator == 0) denominator <- 1
    result <- numerator/denominator
    
    return(result)
  }
  
  #--------------------------------------------------
  
  if(!is.null(confusionM)){
    # Confusion matrix was provided
    
    # Determine class stats
    classes <- seq_len(ncol(confusionM))
    classesDT <- data.table(Class = classes, ClassIndex = seq_along(classes))
    classesDT[, PredN := Matrix::rowSums(confusionM)]
    classesDT[, TruthN := Matrix::colSums(confusionM)]
    classesDT[, `:=`(
      NotPredN = sum(confusionM) - PredN,
      NotTruthN = sum(confusionM) - TruthN
    )]
    
    # Convert confusionM to tall data.table format
    confusion <- as.data.table(confusionM)
    confusion[, PredIndex := .I]
    confusion <- melt(confusion, id.vars = "PredIndex", value.name = "N")
    confusion[, TruthIndex := seq_len(.N), by = PredIndex]
    
  } else{
    # preds and actuals were provided
    
    #--------------------------------------------------
    # If preds and actuals are matrices or data.frames, convert to vectors of the predicted and actual classes
    
    if(is.matrix(preds) | is.data.frame(preds)){
      preds <- as.data.table(preds)
      actuals <- as.data.table(actuals)
      
      if(nrow(preds) != nrow(actuals) | ncol(preds) != ncol(actuals))
        stop("'preds' and 'actuals' should have the same dimensions")
      
      if(!all(as.numeric(as.matrix(preds)) %in% c(0,1)))
        stop("'preds' should only consist of TRUE/FALSE or 1/0")
      
      if(!all(as.numeric(as.matrix(actuals)) %in% c(0,1)))
        stop("'actuals' should only consist of TRUE/FALSE or 1/0")
      
      preds[, RowId := .I]
      preds <- melt(preds, id.vars = "RowId")
      preds <- preds[value == 1][order(RowId)]
      
      actuals[, RowId := .I]
      actuals <- melt(actuals, id.vars = "RowId")
      actuals <- actuals[value == 1][order(RowId)]
      
      preds <- preds$variable
      actuals <- actuals$variable
    }
    
    #--------------------------------------------------
    
    # If preds, actuals are type int, cast to double to prevent integer overflow
    if(is.integer(preds)) preds <- as.double(preds)
    if(is.integer(actuals)) actuals <- as.double(actuals)
    
    # Put the preds and actuals vectors into a data.table
    dt <- data.table(Pred = preds, Truth = actuals)
    
    # Aggregate by (Pred, Truth) pairs
    scores <- dt[, list(N = as.double(.N)), keyby = list(Pred, Truth)]
    
    #--------------------------------------------------
    # Determine the classes
    
    if(is.factor(preds)){
      predclasses <- levels(preds)
    } else{
      predclasses <- unique(scores$Pred)
    }
    if(is.factor(actuals)){
      truthclasses <- levels(actuals)
    } else{
      truthclasses <- unique(scores$Truth)
    }
    classes <- sort(unique(c(predclasses, truthclasses)))
    classesDT <- data.table(Class = classes, ClassIndex = seq_along(classes))
    
    # For each class determine:
    #  PredN = number of predictions of the current class
    #  NotPredN = total number of samples - PredN
    #  TruthN = number of samples of the current class
    #  NotTruthN = total number of samples - TruthN
    predTotals <- scores[, list(PredN = sum(N)), keyby = Pred]
    classesDT[predTotals, PredN := i.PredN, on = c("Class"="Pred")]
    classesDT[is.na(PredN), PredN := 0]
    classesDT[, NotPredN := sum(PredN) - PredN]
    
    truthTotals <- scores[, list(TruthN = sum(N)), keyby = Truth]
    classesDT[truthTotals, TruthN := i.TruthN, on = c("Class"="Truth")]
    classesDT[is.na(TruthN), TruthN := 0]
    classesDT[, NotTruthN := sum(TruthN) - TruthN]
    
    # Build the confusion table (i.e. a tall version of the confusion matrix)
    confusion <- CJ(Pred = classes, Truth = classes)
    confusion[classesDT, PredIndex := i.ClassIndex, on = c("Pred"= "Class")]
    confusion[classesDT, TruthIndex := i.ClassIndex, on = c("Truth"= "Class")]
    confusion[scores, N := i.N, on = c("Pred", "Truth")]
    confusion[is.na(N), N := 0]
    
    # View the confusion matrix
    # dcast(confusion, Pred ~ Truth, value.var = "N", fun.aggregate = function(x) x[1L])
  }
  
  # Solve for the numerator
  numerator <- CJ(k = seq_along(classes), m = seq_along(classes), l = seq_along(classes))
  numerator[confusion, Ckk := i.N, on = c("k" = "PredIndex", "k" = "TruthIndex")]
  numerator[confusion, Clm := i.N, on = c("l" = "PredIndex", "m" = "TruthIndex")]
  numerator[confusion, Ckl := i.N, on = c("k" = "PredIndex", "l" = "TruthIndex")]
  numerator[confusion, Cmk := i.N, on = c("m" = "PredIndex", "k" = "TruthIndex")]
  num <- numerator[, list(sum(Ckk * Clm - Ckl*Cmk))]$V1
  
  # Solve for the two components of the denominator
  denom <- classesDT[, list(D1 = sum(PredN * NotPredN), D2 = sum(TruthN * NotTruthN))]
  denom <- sqrt(denom$D1) * sqrt(denom$D2)
  if(denom == 0) denom <- 1  # Arbitrarily set the denom to 1 if it is 0
  
  # Build the result
  result <- num/denom
  
  return(result)
}
