#' Alien training dataset
#'
#' A dataset describing features of living beings and whether or not they are an alien
#'
#' @format A data.table with 8 rows and 6 variables:
#' \describe{
#'   \item{SkinColor}{Skin color of the individual}
#'   \item{IQScore}{IQ score of the individual}
#'   \item{Cat1}{Categorical descriptor}
#'   \item{Cat2}{Categorical descriptor}
#'   \item{Cat3}{Categorical descriptor}
#'   \item{IsAlien}{Is this being an alien?}
#' }
#' 
#' @details 
#' library(data.table)
#' 
#' alien.train <- data.table::data.table(
#'   SkinColor=c("green", "white", "brown", "white", "blue", "white", "green", "white"),
#'   IQScore=c(300, 95, 105, 250, 115, 85, 130, 115),
#'   Cat1=c("type1", "type1", "type2", "type4", "type2", "type4", "type1", "type1"),
#'   Cat2=c("type1", "type2", "type6", "type5", "type7", "type5", "type2", "type1"),
#'   Cat3=c("type4", "type4", "type11", "type2", "type11", "type2", "type4", "type4"),
#'   IsAlien=c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE)
#' )
#' 
#' save(alien.train, file="data/alien_train.rda")
#' 
"alien.train"


#' Alien test dataset
#'
#' A dataset describing features of living beings
#'
#' @format A data.table with 8 rows and 5 variables:
#' \describe{
#'   \item{SkinColor}{Skin color of the individual}
#'   \item{IQScore}{IQ score of the individual}
#'   \item{Cat1}{Categorical descriptor}
#'   \item{Cat2}{Categorical descriptor}
#'   \item{Cat3}{Categorical descriptor}
#' }
#' 
#' @details 
#' library(data.table)
#' 
#' alien.test <- data.table::data.table(
#'   SkinColor=c("white", "green", "brown", "white", "red"),
#'   IQScore=c(79, 100, 125, 90, 115),
#'   Cat1=c("type4", "type4", "type3", "type1", "type1"),
#'   Cat2=c("type5", "type5", "type9", "type8", "type2"),
#'   Cat3=c("type2", "type2", "type7", "type4", "type4")
#' )
#' 
#' save(alien.test, file="data/alien_test.rda")
#' 
"alien.test"
