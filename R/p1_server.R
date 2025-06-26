#' The application server-side Practical 1
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import ggplot2 dplyr tibble tidyr
#' @importFrom extraDistr dbbinom
#' @importFrom stats dbeta qbeta setNames
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @noRd
p1_server <- function(input, output, session) {

  # For debugging
  # input <- list(a0 = .01, b0 = .01, r = 4)
  # input <- list(a0 = 0, b0 = 0, r = 20)
  plot_style <- list(
    # labs(x = expression(theta), y = NULL, color = NULL),
    scale_color_manual(
      values = setNames(cols$name, cols$target)
    ),
    theme_minimal(),
    theme(
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  )


  ## Update slider maximum limit of slider "r"
  observe({
    sample_size <- input$n
    updateSliderInput(
      session, "r",
      max = sample_size
    )
  })


  dist_summaries <- reactive({
    tibble(
      "-" = c("Prior", "Posterior", "Predictive")
      ,
      Mean = c(
        mean_beta(input$a0, input$b0),
        mean_beta(input$a0 + input$r, input$b0 + input$n - input$r),
        10*mean_beta(input$a0 + input$r, input$b0 + input$n - input$r)
      ) %>%
        round(2)
      ,
      SD = c(
        sd_beta(input$a0, input$b0),
        sd_beta(input$a0 + input$r, input$b0 + input$n - input$r),
        sqrt(10*(input$a0 + input$b0 + 30))*sd_beta(input$a0 + input$r, input$b0 + input$n - input$r)
      ) %>%
        round(2)
      ,
      Q_025 = qbeta(
        0.025, input$a0 + c(0, input$r), input$b0 + c(0, input$n - input$r)
      ) %>%
        round(2) %>%
        c(
          ifelse(
            input$b0 + input$n - input$r > 0,
            qbbinom(.025, n = 10, input$a0 + input$r, input$b0 + input$n - input$r),
            10
          )
        )
      ,
      Q_975 = qbeta(
        0.975, input$a0 + c(0, input$r), input$b0 + c(0, input$n - input$r)
      ) %>%
        round(2) %>%
        c(
          ifelse(
            input$b0 + input$n - input$r > 0,
            qbbinom(.975, n = 10, input$a0 + input$r, input$b0 + input$n - input$r),
            10
          )
        )
    )
  })


  output$inference <- renderPlot(
    data.frame(
      x = seq(0.01, .99, length = 101)
    ) %>%
      mutate(
        prior = dbeta(.data$x, input$a0, input$b0),
        likelihood = dbeta(.data$x, 1 + input$r, 1 + input$n - input$r),
        posterior = dbeta(.data$x, input$a0 + input$r, input$b0 + input$n - input$r)
      ) %>%
      pivot_longer(
        cols = -.data$x,
        names_to = "curve",
        values_to = "y"
      ) %>%
      ggplot(aes(.data$x, .data$y)) +
      geom_area(
        ## 95 % CrI prior
        data = ~ .x %>%
          filter(
            .data$curve == "prior",
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
              .data$x,
              qbeta(0.025, input$a0 + input$r, input$b0 + input$n - input$r),
              qbeta(0.975, input$a0 + input$r, input$b0 + input$n - input$r)
            )
          ),
        fill = "darkgreen",
        alpha = .2
      ) +
      geom_vline(
        data = dist_summaries() %>%
          mutate(curve = tolower(`-`)) %>%
          filter(.data$curve != "predictive"),
        aes(xintercept = .data$Mean, colour = .data$curve),
        lwd = 1,
        alpha = .2,
        show.legend = FALSE
      ) +
      geom_line(aes(colour = .data$curve)) +
      plot_style +
      labs(x = expression(theta), y = NULL, color = NULL)
  )

  output$predictive <- renderPlot(
    tibble(x = 0:10) %>%
      mutate(
        y = dbbinom(.data$x, 10, input$a0 + input$r, input$b0 + input$n - input$r)
      ) %>%
      ggplot(aes(.data$x, .data$y)) +
      geom_bar(
        fill = cols %>% filter(.data$target == "predictive") %>% pull(.data$name),
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
