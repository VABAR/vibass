#' Weights of children
#'
#' Data for the VIBASS session on linear models. This is a simulated dataset
#' that includes data about children. The variables in the dataset are:
#'
#' \itemize{
#'  \item age. Age (in years).
#'  \item vegetables. Measure of vegetables consumption.
#'  \item weight. Weight (in kg).
#'  \item sex. Girl or Boy.
#'  \item height. Height (in cm).
#'  \item ethnicity Asian, Black or European.
#' }
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
#' @source VIBASS Team.
#'
#' @examples
#' data(Weights)
#' summary(Weights)
#'
#' # ML estimates
#' lmW <- lm(weight ~ age, data = Weights)
#' summary(lmW)
NULL
