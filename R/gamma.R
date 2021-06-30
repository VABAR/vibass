lambda_values <- function(a, b) {
  l_range <- qgamma(c(.0005, .999), a, b)

  data.frame(
    x = seq(min(0.01, l_range[1]), l_range[2], length = 101)
  )
}


#' Gamma-Poisson quantile function
#'
#' @param p numeric vector of values between 0 and 1. Cumulative probabilities.
#' @param a numeric vector of alpha values.
#' @param b numeric vector of gamma values.
#'
#' @return numeric vector of quantiles between 0 and n.
#' @importFrom extraDistr pgpois
#'
#' @examples
#' p <- c(0, .2, .5, .9)
#' qgpois(p, 600, 25)
#' pgpois(0:10, 600, 25)
#' @noRd
qgpois <- function(p, a, b) {
  # a <- 625; b <- 25
  stopifnot(
    all(p >= 0),
    all(p <= 1)
  )

  ## Set a reasonable upper-limit for n that will
  ## surely cover the requested probabilities
  max_logit_p <- max(qlogis(p))
  lambda_max <- 2*a/b
  n_max <- qpois(plogis(max_logit_p + 1), lambda = lambda_max)

  n_max <- max(n_max, 1)

  cdf <- pgpois(c(0, seq.int(n_max)), a, b)

  q <- apply(
    vapply(p, '<=', rep(T, length(cdf)), cdf),
    2,
    function(.) head(which(.), 1)
  ) - 1

  return(q)
}


#' Mean of a Gamma-Poisson distribution
#'
#' @param a alpha
#' @param b gamma
#'
#' @return positive numeric value.
#' @noRd
#' @examples
#' mean_gapo(1, 1)
#' mean_gapo(600, 25)
mean_gapo <- function(a, b) {
  mean_gamma(a, b)
}


#' Standard deviation of a Gamma-Poisson distribution
#'
#' @param a alpha
#' @param b gamma
#'
#' @return positive numeric value.
#' @noRd
#' @examples
#' sd_gapo(1, 1)
#' sd_gapo(600, 25)
sd_gapo <- function(a, b) {
  sqrt( a / b * (1 + 1/b) )
}

#' Mean of a Gamma distribution
#'
#' @param a alpha
#' @param b gamma
#'
#' @return positive numeric value.
#' @noRd
#' @examples
#' mean_gamma(1, 1)
#' mean_gamma(600, 25)
mean_gamma <- function(a, b) {
  a / b
}

#' Standard deviation of a Gamma distribution
#'
#' @param a alpha
#' @param b gamma
#'
#' @return positive numeric value.
#' @noRd
#' @examples
#' sd_gamma(1, 1)
#' sd_gamma(600, 25)
sd_gamma <- function(a, b) {
  sqrt( a / b / b )
}

