#' VIBASS interactive apps.
#'
#' Launches the Shiny interactive applications for the practicals.
#'
#' @param x integer 1. Practical id.
#'
#' @import shiny
#' @export
vibass_app <- function (x = 1, bg = FALSE) {

  app <- shinyApp(
    ui = eval(parse(text = paste0("p", x, "_ui"))),
    server = eval(parse(text = paste0("p", x, "_server"))),
    options = list(
      # launch.browser = FALSE,
      # width = "900px",
      # height = "600px"
    )
  )

  app

  # path <- paste0("\"apps/p", x, "\"")
  # code_launch <- paste0(
  #   "shiny::runApp(system.file(",
  #   path, ", package = \"vibass\"), launch.browser = TRUE)"
  # )
  #
  # if (bg) {
  #   system(paste0("Rscript -e '", "vibass::vibass_app(x)", "' &"))
  # } else {
  #   eval(parse(text = code_launch))
  # }
}

