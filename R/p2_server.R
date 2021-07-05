#' The application server-side Practical 2
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import colorspace ggplot2 dplyr tibble tidyr
#' @importFrom stats rbeta dbeta
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @noRd
p2_server <- function(input, output, session) {

  N_samples <- 1e3
  # For debugging
  # input <- list(a_b = 14.5, b_b = 36.5)
  plot_style <- list(
    # labs(x = expression(theta), y = NULL, color = NULL),
    # scale_color_manual(
    #   values = cols %>% pull(name, name = target)
    # ),
    theme_minimal(),
    theme(
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
  )

  samples_red <- data.frame(
    color = "red",
    theta = rbeta(N_samples, 4.5, 16.5)
  ) %>%
    mutate(
      odds = .data$theta / (1 - .data$theta),
      logodds = log(.data$theta / (1 - .data$theta))
    )

  output$transformation_functions <- renderPlot(
    fig_transformations
  )

  samples <- reactive({
    # samples <- function() {  # debug
    x <- rbeta(N_samples, input$a_b, input$b_b)
    odds <-  x/(1-x)
    rbind(
      samples_red,
      data.frame(
        color = "blue",
        theta = x,
        odds = odds,
        logodds = log(odds)
      )
    )
    # }
  })

  output$posteriors <- renderPlot(
    theta_values %>%
      mutate(
        posterior_red = dbeta(x, 4.5, 16.5),
        posterior_blue = dbeta(x, input$a_b, input$b_b)
      ) %>%
      pivot_longer(
        cols = -x,
        names_to = "curve",
        names_prefix = "posterior_",
        values_to = "y"
      ) %>%
      ggplot(aes(.data$x, .data$y)) +
      geom_line(
        aes(colour = .data$curve),
        lwd = 1,
        show.legend = FALSE
      ) +
      plot_style +
      scale_colour_manual(values = mm_cols) +
      labs(x = expression(.data$theta), y = NULL, color = NULL)
  )

  output$theta_b_dist <- renderUI({
    eq <- paste0(
      "$\\theta_b \\sim \\text{Be}(\\alpha_b =",
      input$a_b,
      ",\\, \\beta_b =",
      input$b_b,
      ")$"
    )
    HTML(eq)
  })


  # Histograms --------------------------------------------------------------
  output$hist_theta <- renderPlot(
    ggplot(samples(), aes(.data$theta, fill = .data$color)) +
      geom_histogram(bins = 16, show.legend = FALSE) +
      scale_fill_manual(values = mm_cols) +
      plot_style +
      labs(x = expression(theta), y = NULL, color = NULL) +
      facet_grid(color~., labeller = function(.) list(c("", "")))
  )

  output$hist_odds <- renderPlot(
    ggplot(samples(), aes(odds, fill = .data$color)) +
      geom_histogram(bins = 16, show.legend = FALSE) +
      scale_fill_manual(values = mm_cols) +
      plot_style +
      labs(x = expression(odds(theta)), y = NULL, color = NULL) +
      facet_grid(color~., labeller = function(.) list(c("", "")))
  )

  output$hist_logodds <- renderPlot(
    ggplot(samples(), aes(.data$logodds, fill = .data$color)) +
      geom_histogram(bins = 16, show.legend = FALSE) +
      scale_fill_manual(values = mm_cols) +
      plot_style +
      labs(x = expression(log-odds(theta)), y = NULL, color = NULL) +
      facet_grid(color~., labeller = function(.) list(c("", "")))
  )



  # Contrasts ---------------------------------------------------------------

  output$contrast_diff <- renderPlot(
    samples() %>%
      group_by(.data$color) %>%
      mutate(id = row_number()) %>%
      pivot_wider(
        id_cols = "id",
        names_from = "color",
        values_from = "theta"
      ) %>%
      mutate(x = blue - .data$red) %>%
      ggplot(aes(x)) +
      geom_histogram(bins = 16) +
      geom_vline(xintercept = 0, colour = "darkgrey") +
      plot_style +
      labs(x = expression(theta[b]-theta[r]), y = NULL, color = NULL)
      # + coord_cartesian(xlim = c(-1, 1) * .3)
  )

  output$contrast_ratio <- renderPlot(
    samples() %>%
      group_by(.data$color) %>%
      mutate(id = row_number()) %>%
      pivot_wider(
        id_cols = "id",
        names_from = "color",
        values_from = "theta"
      ) %>%
      mutate(x = .data$blue / .data$red) %>%
      ggplot(aes(x)) +
      geom_histogram(bins = 16) +
      geom_vline(xintercept = 1, colour = "darkgrey") +
      plot_style +
      labs(x = expression(theta[b]/theta[r]), y = NULL, color = NULL)
      # + coord_cartesian(xlim = c(0, 6))
  )

  output$contrast_logratio <- renderPlot(
    samples() %>%
      group_by(.data$color) %>%
      mutate(id = row_number()) %>%
      pivot_wider(
        id_cols = "id",
        names_from = "color",
        values_from = "theta"
      ) %>%
      mutate(x = log(.data$blue / .data$red)) %>%
      ggplot(aes(x)) +
      geom_histogram(bins = 16) +
      geom_vline(xintercept = 0, colour = "darkgrey") +
      plot_style +
      labs(x = expression(log(theta[b]/theta[r])), y = NULL, color = NULL)
      # + coord_cartesian(xlim = c(-1, 2))
  )


}
