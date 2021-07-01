#' VIBASS interactive apps.
#'
#' Launches the Shiny interactive applications for the practicals.
#'
#' @param x integer or character interpretable as integer. 1--3. Practical id.
#'
#' @import shiny
#' @export
vibass_app <- function (x = 1) {

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
}

