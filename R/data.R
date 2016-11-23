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
"alien.test"