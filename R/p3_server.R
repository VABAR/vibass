#' The application server-side Practical 1
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import colorspace ggplot2 dplyr tibble tidyr
#' @importFrom extraDistr dgpois
#' @noRd
p3_server <- function(input, output, session) {

  a0 <- textedSliderServer("a0", 0.5)

  # For debugging
  # input <- list(a0 = .01, b0 = .01, r = 4)
  plot_style <- list(
    # labs(x = expression(lambda), y = NULL, color = NULL),
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


  dist_summaries <- reactive({
    tibble(
      "-" = c("Prior", "Posterior", "Predictive")
      ,
      Mean = c(
        mean_gamma(a0$value, input$b0),
        mean_gamma(a0$value + input$r, input$b0 + 25),
        mean_gapo(a0$value + input$r, input$b0 + 25)
      ) %>%
        round(2)
      ,
      SD = c(
        sd_gamma(a0$value, input$b0),
        sd_gamma(a0$value + input$r, input$b0 + 25),
        sd_gapo(a0$value + input$r, input$b0 + 25)
      ) %>%
        round(2)
      ,
      Q_025 = qgamma(
        0.025, a0$value + c(0, input$r), input$b0 + c(0, 25)
      ) %>%
        round(2) %>%
        c(qgpois(.025, a0$value + input$r, input$b0 + 25))
      ,
      Q_975 = qgamma(
        0.975, a0$value + c(0, input$r), input$b0 + c(0, 25)
      ) %>%
        round(2) %>%
        c(qgpois(.975, a0$value + input$r, input$b0 + 25))
    )
  })


  output$inference <- renderPlot(
    lambda_values(a0$value + input$r, input$b0 + 25) %>%
      mutate(
        prior = dgamma(x, a0$value, input$b0),
        likelihood = dgamma(x, input$r + 1, 25),
        posterior = dgamma(x, a0$value + input$r, input$b0 + 25)
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
              qgamma(0.025, a0$value, input$b0),
              qgamma(0.975, a0$value, input$b0)
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
              qgamma(0.025, a0$value + input$r, input$b0 + 25),
              qgamma(0.975, a0$value + input$r, input$b0 + 25)
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
      labs(x = expression(lambda), y = NULL, color = NULL)
  )

  output$predictive <- renderPlot(
    tibble(x = c(0, seq.int(50))) %>%
      mutate(
        y = dgpois(x, a0$value + input$r, input$b0 + 25)
      ) %>%
      ggplot(aes(x, y)) +
      geom_bar(
        fill = cols %>% filter(target == "predictive") %>% pull(name),
        stat = "identity", width = .2
      ) +
      scale_x_continuous(breaks = c(0, seq.int(50))) +
      plot_style +
      labs(x = "Y'", y = NULL) +
      theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank()
      )
  )


  output$dist_summaries <- renderTable({dist_summaries()})

}
