#' The application User-Interface Practical 2
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
p2_ui <- function(request) {

  # Sidebar with parameter controls -----------------------------------------
  sidebar <- sidebarPanel(
    width = 3,

    titlePanel("Posteriors"),

    helpText(
      "The posterior distribution of red candies is fixed.",
      "Set the parameters for the blue candies. E.g.",
      "$\\alpha_r = 14.5, \\beta_r = 36.5$."
    ),

    sliderInput("a_b", "$\\alpha_b$", value = 14.5, min = 0, max = 50, step = .5),
    sliderInput("b_b", "$\\beta_b$", value = 36.5, min = 0, max = 50, step = .5),

    titlePanel("Simulation"),

    helpText(
      "Random samples of $1\\,000$ values of $\\theta_r$,",
      "$\\text{odds}(\\theta_r)$ and $\\text{log-odds}(\\theta_r)$."
    ),

    titlePanel("Contrast"),

    helpText(
      "Posterior difference, ratio and log-ratio of the previous",
      "samples of $\\theta_b$ and $\\theta_r$."
    )
  )


  # Main body with plots and results ----------------------------------------
  body <- mainPanel(
    width = 9,

    fluidRow(
      column(
        4,
        plotOutput("posteriors", height = "200px")
      ),
      column(
        4,
        helpText(
          "$\\theta_b \\sim \\text{Be}(\\alpha_b, \\beta_b)$"
        ),
        helpText(
          "$\\theta_r \\sim \\text{Be}(\\alpha_r = 4.5, \\beta_r = 16.5)$"
        ),
        # ,
        # uiOutput("theta_b_dist")
        helpText(
          "$\\text{odds}(\\theta) = \\frac{\\theta}{1-\\theta}$"
        ),
        helpText(
          "$\\text{log-odds}(\\theta) = \\log\\frac{\\theta}{1-\\theta}$",
        )
      ),
      column(
        4,
        plotOutput("transformation_functions", width = "200px", height = "200px"),

      )
    ),

    fluidRow(
      column(
        4,
        plotOutput("hist_theta", height = "300px")
      ),
      column(
        4,
        plotOutput("hist_odds", height = "300px")
      ),
      column(
        4,
        plotOutput("hist_logodds", height = "300px")
      )
    ),

    fluidRow(
      column(
        4,
        plotOutput("contrast_diff", height = "200px")
      ),
      column(
        4,
        plotOutput("contrast_ratio", height = "200px")
      ),
      column(
        4,
        plotOutput("contrast_logratio", height = "200px")
      )
    ),

    fluidRow(
      column(
        4,
        tableOutput("sumtable_diff")
      ),
      column(
        4,
        tableOutput("sumtable_ratio")
      ),
      column(
        4,
        tableOutput("sumtable_logratio")
      )
    )
  )

  # outline -----------------------------------------------------------------
  tagList(
    # Leave this function for adding external resources
    add_external_resources(1),

    # First level UI elements here

    # input <- list(a0 = .01, b0 = .01, r = 4)
    fluidPage(

      withMathJax(),
      # section below allows in-line LaTeX via $ in mathjax. Replace less-than-sign with <
      # and grater-than-sign with >
      tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script>
                ")),

      titlePanel("Practical 2: Inference with simulated samples"),

      strong(
        paste(
          "Estimate the distribution of functions of parameters",
          "by Monte Carlo simulation."
        )
      ),

      sidebarLayout(sidebar, body)
    )

  )

}
