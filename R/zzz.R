.onAttach <- function(...) {

  packageStartupMessage(
    print_welcome()
  )
}

print_welcome <- function() {
  message(
    text_col(
      cli::rule(
        left = crayon::bold("Welcomme to VIBASS"),
        right = paste("vibass", packageVersion("vibass"))
      )
    )
  )

  message(
    "Training materials for the introductory ",
    "course on Bayesian inference, at the València ",
    "International Bayesian Summer School."
  )

  message(
    crayon::blue("Use:\n"),
    paste(
      format(
        text_col(
          c("browseVignettes() ",
            "vignette('p1') ",
            "vibass_app('1') "
          )
        )
      ),
      c(
        "to access the course practicals\n",
        "to open a practical (e.g. the first one)\n",
        "to open an interactive app (e.g. the first one)"
      )
    )
  )

  message(text_col(cli::rule()))
}
