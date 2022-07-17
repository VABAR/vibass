#' VIBASS interactive apps.
#'
#' Launches the Shiny interactive applications for the practicals.
#'
#' @param x integer or character interpretable as integer. See
#'   [get_available_apps()] for valid options.
#'
#' @import shiny
#' @export
vibass_app <- function (x = NULL) {

  if (is.null(x)) {
    return(message(available_apps_message()))
  }

  if (length(x) != 1L || !as.character(x) %in% available_apps()) {
    stop(
      "Invalid app code.\n",
      available_apps_message()
    )
  }

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

#' Return a string with the apps available.
#'
#' @return Character string.
#' @keywords internal
available_apps_message <- function() {
  paste(
    "Available apps:",
    paste(available_apps(), collapse = ", ")
  )
}

#' List available apps in {vibass} package.
#'
#' App codes that are available for use in [vibass_app()].
#'
#' @return Character vector.
#' @export
#'
#' @examples
#' available_apps()
available_apps <- function() {
  ns_vibass <- names(getNamespace('vibass'))
  app_basenames <- ns_vibass[grep("p.+_server", ns_vibass)]
  ans <- sort(gsub('_server', '', gsub('^p', '', app_basenames)))
  return(ans)
}
