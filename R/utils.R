#' Return text in a contrasting colour
#'
#' Overcome default colouring schemes (e.g. for packageStartupMessages())
#' and make sure some text is printed in a contrasting colour depending
#' on the theme (dark or light) in RStudio.
#'
#' Uses \code{cli} for printing white text in dark themes or black text
#' on light themes in RStudio.
#' Borrowed from package \code{tidyverse} (https://github.com/tidyverse/tidyverse/blob/72af810106d7249c905d6b0f5b8b42dc33e6ac21/R/utils.R)
#'
#' @param x Character. Text to print.
#'
#' @examples
#' vibass:::text_col("Hello world")
#' message("Hello world")
#' message(vibass:::text_col("Hello world"))
text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) cli::col_white(x) else cli::col_black(x)

}
