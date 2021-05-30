

# Top banner with explanatory text and equations --------------------------
explanations <- fluidRow(

  column(
    3,
    helpText(
      span(
        strong("Prior"),
        style = paste0(
          "color:",
          cols %>% filter(target == "prior") %>% pull(hex)
        )
      ),
      "distribution $\\theta \\sim \\text{Be}(\\alpha_0,\\, \\beta_0)$.",
      br(),
      "Represents your beliefs", strong("before"), "observing the data $r$.",
      br(),
      "The shaded areas represent central 95% Credible Intervals."
    )
  ),

  column(
    3,
    helpText(
      span(
        strong("Likelihood"),
        style = paste0(
          "color:",
          cols %>% filter(target == "likelihood") %>% pull(hex)
        )
      ),
      ": $$L(\\theta \\mid \\mathcal D) = P(Y=r \\mid \\theta,\\, n)$$",
      "where $Y \\sim \\text{Bi}(n,\\, \\theta)$ and $n = 20$.", br(),
      # "It is a", strong("function"), "of $\\theta$, not a probability distribution. ",
      # "Here it is scaled for visualisation purposes.",
      "Represents the relative", strong("compatibility"),
      "of $\\theta$ values with the observed $r$."
    )
  ),

  column(
    3,
    helpText(
      span(
        strong("Posterior"),
        style = paste0(
          "color:",
          cols %>% filter(target == "posterior") %>% pull(hex)
        )
      ),
      ": $$\\theta \\mid r \\sim \\text{Be}(\\alpha_0 + r,\\, \\beta_0 + n - r)$$",
      "where $n = 20$.", br(),
      "Represents your knowledge about $\\theta$", strong("after"),
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
          cols %>% filter(target == "predictive") %>% pull(hex)
        )
      ),
      ": $$Y' \\sim \\text{Bb}(\\alpha_0 + r,\\, \\beta_0 + n - r,\\, n')$$",
      "where $n' = 10$.", br(),
      "Represents the predicted outcome of a future observation."
    )
  )
)


# Sidebar with parameter controls -----------------------------------------
sidebar <- sidebarPanel(
  width = 3,

  titlePanel("Parameter control"),
  sliderInput("a0", "$\\alpha_0$", value = 0.5, min = 0, max = 100, step = .5),
  sliderInput("b0", "$\\beta_0$", value = 0.5, min = 0, max = 100, step = .5),

  helpText(
    strong("Observed"), "number of red M&Ms in the sample."
  ),
  sliderInput("r", "$r$", value = 4, min = 0, max = 20, step = 1)

)


# Main body with plots and results ----------------------------------------
body <- mainPanel(
  width = 9,

  fluidRow(
    column(
      6,
      plotOutput("figure", height = "300px")
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
      img(src='MM.png', width = '100%', align = "center")
    )
  )
)

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

  titlePanel("Practical 1: Counting red M&Ms"),

  strong(
    paste(
      "Estimate the global proportion $\\boldsymbol{\\theta}$ of red M&Ms",
      "by observing their number in a sample of size 20."
    )
  ),

  explanations,

  sidebarLayout(sidebar, body)
)
