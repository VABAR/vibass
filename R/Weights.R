#' Weights of children
#'
#' Data for the VIBASS session on linear models. The dataset include
#' data bout children, that includes weight, height and some other variables.
#'
#' @name Weights
#'
#' @docType data
#'
#' @usage data("Weights")
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets
#'
#' @references To be added.
#'
#' @source To be added.
#'
#' @examples
#' data(Weights)
#' summary(Weights)
#' 
#' # ML estimates
#' lmW <- lm(weight ~ age, data = Weights)
#' summary(lmW) 
NULL
