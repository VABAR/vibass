#' VIBASS interactive apps.
#'
#' Launches the Shiny interactive applications for the practicals.
#'
#' @param x integer 1. Practical id.
#'
#' @import shiny extraDistr colorspace ggplot2 dplyr tibble tidyr
#' @importFrom magrittr %>%
#' @export
vibass_app <- function (x = 1, bg = FALSE) {

  path <- paste0("\"apps/p", x, "\"")
  code_launch <- paste0(
    "shiny::runApp(system.file(",
    path, ", package = \"vibass\"), launch.browser = TRUE)"
  )

  if (bg) {
    system(paste("Rscript -e '", code_launch, "' &"))
  } else {
    eval(parse(text = code_launch))
  }
}

