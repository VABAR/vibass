theta_values <- data.frame(
  x = seq(0.01, .99, length = 101)
)


#' Beta-Binomial quantile function
#'
#' @param p numeric vector of values between 0 and 1. Cumulative probabilities.
#' @param n integer vector of number of observations.
#' @param a numeric vector of alpha values.
#' @param b numeric vector of beta values.
#'
#' @return numeric vector of quantiles between 0 and n.
#' @importFrom extraDistr pbbinom
#'
#' @examples
#' p <- c(0, .2, .5, .9)
#' qbbinom(p, 10, .2, .4)
#' pbbinom(0:10, 10, .2, .4)
#' @noRd
qbbinom <- function(p, n, a, b) {
  # n <- 10; a <- 5; b <- 8
  stopifnot(
    all(p >= 0),
    all(p <= 1)
  )
  cdf <- if(a > 0) {
    pbbinom(0:n, n, a, b)
  } else {
    # extreme case
    rep(1, n+1)
  }

  q <- apply(
    vapply(p, '<=', rep(T, length(cdf)), cdf),
    2,
    function(.) head(which(.), 1)
  ) - 1

  return(q)
}


#' Mean of a Beta distribution
#'
#' @param a alpha
#' @param b beta
#'
#' @return numeric value between 0 and 1.
#' @noRd
#' @examples
#' mean_beta(1, 1)
#' mean_beta(5, 5)
mean_beta <- function(a, b) {
  a / (a + b)
}

#' Standard deviation of a Beta distribution
#'
#' @param a alpha
#' @param b beta
#'
#' @return numeric value between 0 and 1.
#' @noRd
#' @examples
#' sd_beta(1, 1)
#' sd_beta(5, 5)
sd_beta <- function(a, b) {
  sqrt( a * b / ( (a + b)**2 * (a + b + 1) ) )
}
