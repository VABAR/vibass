#' Print a standardised summary table
#'
#' Make a table of several summary statistics with proper formatting.
#'
#' The table includes the mean, variance and standard deviation, a vector of 3
#' quantiles at 0.05, 0.50 and 0.95, a 95% centred confidence interval and a
#' numeric value to be interpreted as a proportion above 0 and 1. All values are
#' rounded to the specified number of decimal places.
#'
#' @param mean Real.
#' @param var Real.
#' @param quant Named numeric vector. Names must be of the form "xx%" with
#'   numeric xx. As from the output of the function \code{quantile}.
#' @param ic95 Numeric vector.
#' @param prop0 Real.
#' @param prop1 Real.
#' @param label Character. Name of the summarised variable.
#' @param digits Integer. Number of decimal places to be used.
#'
#' @return
#' @importFrom knitr kable
#' @importFrom tibble rownames_to_column
#' @export
#'
#' @examples
#' summary_table(mean = 1, var = 1, quant = quantile(1:10, 0:4/4), ic95 = 4:5,
#' prop1 = .6, label = "test")
summary_table <- function(
  mean,
  var,
  quant,
  ic95 = NULL,
  prop0 = NULL,
  prop1 = NULL,
  label,
  digits = 2
) {

  quantiles <- as.numeric(gsub("%", "", names(quant)))

  extremes <- quantiles %in% c(0, 100)

  quantile_names <- c(
    gsub("0%", "Min.", names(quant))[quantiles == 0],
    paste0("Q", formatC(quantiles[!extremes], width = 2, flag = "0")),
    gsub("100%", "Max.", names(quant))[quantiles == 100]
  )

  prettyNum(quantiles[!idx_100], digits = 2, zero.print = "0", replace.zero = TRUE)

  c(
    round( c(mean, var, sqrt(var), quant), digits),
    if(!is.null(ic95)) {paste0("(", paste(round(ic95, digits), collapse = ", "), ")")},
    if(!is.null(prop0)) {paste0(round(100*prop0, 1), "%")},
    if(!is.null(prop1)) {paste0(round(100*prop1, 1), "%")}
  ) |>
    setNames(
      c(
        "Mean", "Var", "S.Dev",
        quantile_names,
        if(!is.null(ic95)) {"IC95"},
        if(!is.null(prop0)) {"Prop>0"},
        if(!is.null(prop1)) {"Prop>1"}
      )
    ) |>
    # as.list() |>
    data.frame() |>
    rownames_to_column() |>
    setNames(c("Summary", label)) |>
    kable()
}
