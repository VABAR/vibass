#' The application server-side Practical 5
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import ggplot2 dplyr tibble tidyr
#' @importFrom extraDistr dbbinom
#' @importFrom stats dbeta qbeta setNames
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @noRd

# Load data
data <- data.frame(MMs = c(20, 22, 24), red = c(5, 8, 9))

values <-  reactiveValues(theta_sim = NA, ww = NA, density = NA, LL = NA,
  UL = NA)


p5_server <- function(input, output, session) {


  # Run simulations
  IS <- reactive({
    #print("---- IS ----")
    n_simulations <- input$nsim
    param1 <- input$sampa0
    param2 <- input$sampb0

    # FIXME: Handle sampling distribution
    values$theta_sim <- rbeta(n_simulations, param1, param2)

    # Log-Likelihood (for each value of theta_sim)
    loglik_binom <- sapply(values$theta_sim, function(THETA) {
     sum(dbinom(data$red, data$MMs, THETA, log = TRUE))
    })

    # Weights 
    log_ww <- loglik_binom + dbeta(values$theta_sim, input$a0, input$b0, log = TRUE) - dbeta(values$theta_sim, param1, param2, log = TRUE)
    log_ww <- log_ww - max(log_ww)
    ww <- exp(log_ww)
    values$ww <- ww / sum(ww)

    # 95% approximate credible interval
     idx <- order(values$theta_sim)
    # Empirical cumulative 
    theta_sim_ordered <- values$theta_sim[idx]
    aux <- cumsum(values$ww[idx])
    values$LL <- theta_sim_ordered[which.min(abs(aux - 0.025))]
    values$UL <- theta_sim_ordered[which.min(abs(aux - 0.975))]


    # Density
    values$density <- density(values$theta_sim, weights = values$ww)
  })

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

  dist_summaries <- reactive({
    IS()
    tibble(
      "-" = c("Prior", "Posterior", "IS posterior")
      ,
      Mean = c(
        mean_beta(input$a0, input$b0),
        mean_beta(input$a0 + sum(data$red), input$b0 + sum(data$MMs - data$red)),
        sum(values$theta_sim * values$ww)
      ) %>%
        round(2)
      ,
      SD = c(
        sd_beta(input$a0, input$b0),
        sd_beta(input$a0 + sum(data$red), input$b0 + sum(data$MMs - data$red)),
        sqrt(sum(values$theta_sim^2 * values$ww) - sum(values$theta_sim * values$ww)^2)
      ) %>%
        round(2)
      ,
      Q_025 = qbeta(
        0.025, input$a0 + c(0, input$r), input$b0 + c(0, 20 - input$r)
      ) %>%
        round(2) %>%
        c(
          ifelse(
            input$b0 + sum(data$MMs - data$red) > 0,
            qbbinom(.025, n = sum(data$MMs), input$a0 + sum(data$red), input$b0 + sum(data$MMs - data$red)) / sum(data$MMs),
            sum(data$MMs) / sum(data$MMs)
          )
        ) %>%
          c(values$LL)
       
      ,
      Q_975 = qbeta(
        0.975, input$a0 + c(0, input$r), input$b0 + c(0, 20 - input$r)
      ) %>%
        round(2) %>%
        c(
          ifelse(
            input$b0 + sum(data$MMs - data$red) > 0,
            qbbinom(.975, n = sum(data$MMs), input$a0 + sum(data$red), input$b0 + sum(data$MMs - data$red)) / sum(data$MMs),
             sum(data$MMs) / sum(data$MMs)
          ) %>%
            c(values$UL)
        ),
      ESS = c(NA, NA, sum(values$ww)^2 / sum(values$ww^2))
    )
  })


  output$inference <- renderPlot(
    theta_values %>%
      mutate(
        prior = dbeta(.data$x, input$a0, input$b0),
        likelihood = dbeta(.data$x, 1 + sum(data$red), 1 + sum(data$MMs) - sum(data$red)),
        posterior = dbeta(.data$x, input$a0 + sum(data$red), 1 + sum(data$MMs) - sum(data$red))
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
          filter(.data$curve != "predictive"),
        aes(xintercept = .data$Mean, colour = .data$curve),
        lwd = 1,
        alpha = .2,
        show.legend = FALSE
      ) +
      geom_line(aes(colour = .data$curve)) +
      plot_style +
      labs(x = expression(theta), y = NULL, color = NULL) +
      # Density plot from IS
      geom_line(data = data.frame(x = values$density$x, y = values$density$y),
        aes(x = x, y = y))
  )


  output$weights <- renderPlot(

    qplot(values$ww, geom = "histogram", bins = 40) + xlab("Weight") + 
      ggtitle("Importance sampling weights")
  )

  output$predictive <- renderPlot(
    tibble(x = 0:10) %>%
      mutate(
        y = dbbinom(.data$x, 10, input$a0 + input$r, input$b0 + 20 - input$r)
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
