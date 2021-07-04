#' The application User-Interface Practical 3
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
p3_ui <- function(request) {

  # Top banner with explanatory text and equations --------------------------
  explanations <- fluidRow(

    column(
      3,
      helpText(
        span(
          strong("Prior"),
          style = paste0(
            "color:",
            cols[cols$target == "prior", "hex"]
          )
        ),
        "$$\\lambda \\sim \\text{Ga}(\\alpha_0,\\, \\beta_0)$$",
        "Represents your beliefs", strong("before"),
        "observing the data $r = \\sum_i y_i$.",
        "$\\alpha_0 = 0.5,\\, \\beta_0 = 0$ yields the",
        "improper prior $\\lambda^{-1/2}$.",
        br(),
        "The shaded areas represent central 95% credible intervals."
      )
    ),

    column(
      3,
      helpText(
        span(
          strong("Likelihood"),
          style = paste0(
            "color:",
            cols[cols$target == "likelihood", "hex"]
          )
        ),
        "$$L(\\lambda \\mid \\mathcal D) = P(\\sum_i Y_i = r \\mid \\lambda,\\, n)$$",
        "where $Y_i \\mid \\lambda \\sim \\text{Po}(\\lambda)$ and $n = 25$.", br(),
        # "It is a", strong("function"), "of $\\lambda$, not a probability distribution. ",
        # "Here it is scaled for visualisation purposes.",
        "Represents the relative", strong("compatibility"),
        "of $\\lambda$ values with the observed $r$."
      )
    ),

    column(
      3,
      helpText(
        span(
          strong("Posterior"),
          style = paste0(
            "color:",
            cols[cols$target == "posterior", "hex"]
          )
        ),
        "$$\\lambda \\mid r \\sim \\text{Ga}(\\alpha_0 + r,\\, \\beta_0 + n)$$",
        "where $n = 25$.", br(),
        "Represents your knowledge about $\\lambda$", strong("after"),
        "observing the data $r$."
      )
    ),

    column(
      3,
      helpText(
        span(
          strong("Posterior predictive"),
          style = paste0(
            "color:",
            cols[cols$target == "predictive", "hex"]
          )
        ),
        "$$Y' \\sim \\text{GaPo}(\\alpha_0 + r,\\, \\beta_0 + n,\\, 1)$$", br(),
        "Represents the predicted outcome of future observations."
      )
    )
  )


  # Sidebar with parameter controls -----------------------------------------
  sidebar <- sidebarPanel(
    width = 3,

    titlePanel("Parameter control"),
    textedSliderInput(
      "a0",
      "$\\alpha_0$",
      min = 0,
      max = 110,
      value = 0.5,
      step = .2,
      size = "70px"
    ),
    # sliderInput("a0", "$\\alpha_0$", value = 0.5, min = 0, max = 110, step = .2),
    sliderInput("b0", "$\\beta_0$", value = 0, min = 0, max = 50, step = .2),

    helpText(
      strong("Observed"),
      "total number of u's in the $n = 25$ pages sampled."
    ),
    # sliderInput("r", "$r$", value = 643, min = 0, max = 50*input$b0, step = 1)
    sliderInput("r", "$r$", value = 643, min = 0, max = 50*25, step = 1)

  )


  # Main body with plots and results ----------------------------------------
  body <- mainPanel(
    width = 9,

    fluidRow(
      column(
        6,
        plotOutput("inference", height = "300px")
      ),
      column(
        6,
        plotOutput("predictive", height = "300px")
      )
    ),

    fluidRow(
      column(
        6,
        tableOutput("dist_summaries")
      ),
      column(
        6,
        img(src='www/got.jpg', width = '100%', align = "center")
      )
    )
  )

  tagList(
    # Leave this function for adding external resources
    add_external_resources(3),

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

      titlePanel("Practical 3: Count data"),

      strong(
        "Estimate the number of u's in a page of",
        em("A Game of Thrones.")
      ),

      explanations,

      sidebarLayout(sidebar, body)
    )

  )

}
