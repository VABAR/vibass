#' The application server-side Practical 1
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import colorspace ggplot2 dplyr tibble tidyr
#' @importFrom extraDistr dbbinom
#' @noRd
p1_server <- function(input, output, session) {

  # For debugging
  # input <- list(a0 = .01, b0 = .01, r = 4)
  plot_style <- list(
    # labs(x = expression(theta), y = NULL, color = NULL),
    scale_color_manual(
      values = cols %>% pull(name, name = target)
    ),
    theme_minimal(),
    theme(
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  )

  mean_beta <- function(a, b) a / (a + b)
  sd_beta <- function(a, b) sqrt( a * b / ( (a + b)**2 * (a + b + 1) ) )

  dist_summaries <- reactive({
    tibble(
      "-" = c("Prior", "Posterior", "Predictive")
      ,
      Mean = c(
        mean_beta(input$a0, input$b0),
        mean_beta(input$a0 + input$r, input$b0 + 20 - input$r),
        10*mean_beta(input$a0 + input$r, input$b0 + 20 - input$r)
      ) %>%
        round(2)
      ,
      SD = c(
        sd_beta(input$a0, input$b0),
        sd_beta(input$a0 + input$r, input$b0 + 20 - input$r),
        sqrt(10*(input$a0 + input$b0 + 30))*sd_beta(input$a0 + input$r, input$b0 + 20 - input$r)
      ) %>%
        round(2)
      ,
      Q_025 = qbeta(
        0.025, input$a0 + c(0, input$r), input$b0 + c(0, 20 - input$r)
      ) %>%
        round(2) %>%
        c(qbbinom(.025, n = 10, input$a0 + input$r, input$b0 + 20 - input$r))
      ,
      Q_975 = qbeta(
        0.975, input$a0 + c(0, input$r), input$b0 + c(0, 20 - input$r)
      ) %>%
        round(2) %>%
        c(qbbinom(.975, n = 10, input$a0 + input$r, input$b0 + 20 - input$r))
    )
  })


  output$figure <- renderPlot(
    theta_values %>%
      mutate(
        prior = dbeta(x, input$a0, input$b0),
        likelihood = dbinom(input$r, size = 20, prob = x) /
          # scale it to make it comparable in size
          integrate(function(x) dbinom(input$r, size = 20, prob = x), 0, 1)$value,
        posterior = dbeta(x, input$a0 + input$r, input$b0 + 20 - input$r)
      ) %>%
      pivot_longer(
        cols = -x,
        names_to = "curve",
        values_to = "y"
      ) %>%
      ggplot(aes(x, y)) +
      geom_area(
        ## 95 % CrI prior
        data = ~ .x %>%
          filter(
            curve == "prior",
            between(
              x,
              qbeta(0.025, input$a0, input$b0),
              qbeta(0.975, input$a0, input$b0)
            )
          ),
        fill = "dodgerblue",
        alpha = .2
      ) +
      geom_area(
        ## 95 % CrI posterior
        data = ~ .x %>%
          filter(
            curve == "posterior",
            between(
              x,
              qbeta(0.025, input$a0 + input$r, input$b0 + 20 - input$r),
              qbeta(0.975, input$a0 + input$r, input$b0 + 20 - input$r)
            )
          ),
        fill = "darkgreen",
        alpha = .2
      ) +
      geom_vline(
        data = dist_summaries() %>%
          mutate(curve = tolower(`-`)) %>%
          filter(curve != "predictive"),
        aes(xintercept = Mean, colour = curve),
        lwd = 1,
        alpha = .2,
        show.legend = FALSE
      ) +
      geom_line(aes(colour = curve)) +
      plot_style +
      labs(x = expression(theta), y = NULL, color = NULL)
  )

  output$predictive <- renderPlot(
    tibble(x = 0:10) %>%
      mutate(
        y = dbbinom(x, 10, input$a0 + input$r, input$b0 + 20 - input$r)
      ) %>%
      ggplot(aes(x, y)) +
      geom_bar(
        fill = cols %>% filter(target == "predictive") %>% pull(name),
        stat = "identity", width = .2
      ) +
      scale_x_continuous(breaks = 0:10) +
      plot_style +
      labs(x = "Y'", y = NULL) +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()
      )
  )


  output$dist_summaries <- renderTable({dist_summaries()})

}
