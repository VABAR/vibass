#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
add_external_resources <- function(x){

  www_path <- app_sys(paste0("apps/p", x, "/www"))

  add_resource_path('www', www_path)

  tags$head(
    favicon(),
    bundle_resources(
      path = www_path,
      app_title = paste('VIBASS Practical', x)
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
