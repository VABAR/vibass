#' @import R2BayesX
#' @import lme4


.onAttach <- function(...) {

  packageStartupMessage(
    if (interactive()) print_welcome()
  )
}

#' @importFrom utils packageVersion
#' @import cli
print_welcome <- function() {
  message(
    text_col(
      cli::rule(
        left = cli::style_bold("Welcomme to VIBASS"),
        right = paste("vibass", packageVersion("vibass"))
      )
    )
  )

  cli::cli_par()
  cli::cli_text(
    "Training materials for the introductory ",
    "course on Bayesian inference, at the Val\u00e8ncia ",
    "International Bayesian Summer School."
  )
  cli::cli_end()

  cli::cli_par()
  cli::cli_alert_info(
    c(
      "Browse to: {.url http://vabar.es/vibass/} ",
      cli::col_yellow("to access the course practicals under 'Articles'")
    )
  )

  cli::cli_alert_success(
    c(
      "Use: {.code vibass_app(1)} ",
      cli::col_yellow("to open an interactive app (e.g. the first one)")
    )
  )

  message(text_col(cli::rule()))
}
