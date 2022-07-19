#' The application User-Interface Practical 5
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
p5MH_ui <- function(request) {

  # Top banner with explanatory text and equations --------------------------
  explanations <- fluidRow(

    column(
      3,
      helpText(
        span(
          strong("Prior"),
          style = paste0(
            "color:",
            cols2MH[cols2MH$target == "prior", "hex"]
          )
        ),
        "$$\\theta \\sim \\text{Be}(\\alpha_0,\\, \\beta_0)$$",
        "Represents your beliefs", strong("before"), "observing the data.",
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
            cols2MH[cols2MH$target == "likelihood", "hex"]
          )
        ),
        "$$L(\\theta \\mid \\mathcal D) = \\Pi_{i=1}^3 P(Y=r_i \\mid \\theta,\\, n_i)$$",
        "where $Y \\mid \\theta \\sim \\text{Bi}(n_i,\\, \\theta)$.", br(),
        # "It is a", strong("function"), "of $\\theta$, not a probability distribution. ",
        # "Here it is scaled for visualisation purposes.",
        "Represents the relative", strong("compatibility"),
        "of $\\theta$ values with the observed values $r_i$ among $n_i$ M&M's."
      )
    ),

    column(
      3,
      helpText(
        span(
          strong("Posterior"),
          style = paste0(
            "color:",
            cols2MH[cols2MH$target == "posterior", "hex"]
          )
        ),
        "$$\\theta \\mid r \\sim \\text{Be}(\\alpha_0 + \\sum_{i=1}^3 r_i,\\, \\beta_0 + \\sum_{i=1}^3 (n_i - r_i))$$",
        br(),
        "Represents your knowledge about $\\theta$", strong("after"),
        "observing the data."
      )
    ),

  column(
      3,
      helpText(
        span(
          strong("Proposal distribution"),
          style = paste0(
            "color:",
            cols2MH[cols2MH$target == "posterior_MH", "hex"]
          )
        ),
        "$$\\theta \\sim \\text{Be}(\\alpha_0^{'},\\, \\beta^{'}_0 )$$",
        br(),
        "This distribution will be used to propose new values of $\\theta$.",
        br(),
        "Note that these values can be accepted or rejected. Check ",
        strong("% accepted"), " in the summary table for the proportion of accepted values."
      )
    )



  )


  # Sidebar with parameter controls -----------------------------------------
  sidebar <- sidebarPanel(
    width = 3,

    titlePanel("Parameter control (prior)"),
    sliderInput("a0", "$\\alpha_0$", value = 1, min = 0, max = 100, step = .5),
    sliderInput("b0", "$\\beta_0$", value = 1, min = 0, max = 100, step = .5),
    titlePanel("Parameter control (sampling)"),
    selectInput("samplingdist", "Proposal distribution",
      #c("beta", "lognormal"), selected = "beta"),
      c("beta"), selected = "beta"),
    sliderInput("sampa0", "$\\alpha_0^{'}$", value = 1, min = 0, max = 100, step = .5),
    sliderInput("sampb0", "$\\beta_0^{'}$", value = 1, min = 0, max = 100, step = .5),
    sliderInput("initial", "Initial value", value = 0.5, min = 0.00,
      max = 1, step = 0.01),
    sliderInput("thinning", "Thinning", value = 1, min = 1,
      max = 20, step = 1),
    sliderInput("burnin", "Burn-in", value = 100, min = 0,
      max= 1000, step = 100),
    sliderInput("nsim", "Number of simulations", value = 1000, min = 1000,
      max= 50000, step = 100)

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
        plotOutput("trace", height = "300px")
      )
    ),

    fluidRow(
      column(
        6,
        tableOutput("dist_summaries")
      ),
      column(
        6,
        img(src='www/MM.png', width = '100%', align = "center")
      )
    )
  )

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

      titlePanel("Practical 5: Metropolis-Hastings for counting red M&Ms"),

      strong(
        paste(
          "Estimate the global proportion $\\boldsymbol{\\theta}$ of red M&Ms",
          "by observing their numbers in three outcomes."
        )
      ),

      explanations,

      sidebarLayout(sidebar, body)
    )

  )

}
