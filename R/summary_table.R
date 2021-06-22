#' Print a standardised summary table
#'
#' Make a table of several summary statistics with proper formatting.
#'
#' The table includes the mean, variance and standard deviation, a vector of 3
#' quantiles at 0.05, 0.50 and 0.95, a 95% centred confidence interval and a
#' numeric value to be interpreted as a proportion above 0 and 1. All values are
#' rounded to 2 significant digits.
#'
#' @param mean Real.
#' @param var Real.
#' @param quant Numeric vector.
#' @param ic95 Numeric vector.
#' @param prop0 Real.
#' @param prop1 Real.
#' @param label Character. Name of the summarised variable .
#'
#' @return
#' @export
#'
#' @examples
#' summary_table(mean = 1, var = 1, quant = 1:3, ic95 = 4:5, prop1 = .6, label = "test")
summary_table <- function(mean, var, quant, ic95, prop0 = NULL, prop1 = NULL, label) {
c(1, 2, if(!is.null(NULL)) {paste(round(100*NULL, 2), "%")}, 3)

  c(
    round( c(mean, var, sqrt(var), quant), 2),
    paste0("(", paste(round(ic95, 2), collapse = ", "), ")"),
    if(!is.null(prop0)) {paste0(round(100*prop0, 1), "%")},
    if(!is.null(prop1)) {paste0(round(100*prop1, 1), "%")}
  ) |>
    setNames(
      c("Mean", "Var", "S.Dev", "Q05", "Q50", "Q95", "IC95",
        if(!is.null(prop0)) {"Prop>0"},
        if(!is.null(prop1)) {"Prop>1"})
    ) |>
    # as.list() |>
    data.frame() |>
    rownames_to_column() |>
    setNames(c("Summary", label)) |>
    kable()
}
