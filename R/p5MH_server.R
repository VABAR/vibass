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

values <-  reactiveValues(theta_sim = NA, accepted = NA, density = NA, LL = NA,
  UL = NA)


p5_server <- function(input, output, session) {


  # Run simulations
  MH <- reactive({
    print("---- M-H ----")
    n.iter <- input$nsim
    param1 <- input$sampa0
    param2 <- input$sampb0

    # FIXME: Handle sampling distribution

    if(input$samplingdist == "beta") {

      #Proposal distribution: sampling
      rq <- function() {
        rbeta(1, param1, param2)
      }

      #Proposal distribution: log-density
      logdq <- function(new.theta, theta) {
        dbeta(new.theta, param1, param2, log = TRUE)
      }

    } else { #lognormal: mean, precision are the parameters

      #Proposal distribution: sampling
      rq <- function(theta) {
        rlnorm(1, meanlog = param1, sdlog = sqrt(1 / param2))
      }

      #Proposal distribution: log-density
      logdq <- function(new.theta, theta) {
        dlnorm(new.theta, meanlog = param1, sdlog = sqrt(1 / param2),
          log = TRUE)
      }
    }

    #Prior distribution: Beta(a0, b0)
    logprior <- function(theta) {
      dbeta(theta, input$a0, input$b0, log = TRUE)
    }

    #Log-Likelihood
    loglik <- function(y, theta, N) {
      res <- sum(dbinom(y, N, theta, log = TRUE)) 
    }

    theta <- rep(NA, n.iter)
    accepted <- rep(FALSE, n.iter)

    #Initial value
    theta[1] <- input$initial

    #Data
    y <- data$red
    N <- data$MMs

    for(i in 2:n.iter) {
      new.theta <- rq()

      #Log-Acceptance probability
      logacc.prob <- loglik(y, new.theta, N) + logprior(new.theta) + logdq(theta[i - 1])
      logacc.prob <- logacc.prob - loglik(y, theta[i - 1], N) - logprior(theta[i - 1]) - 
        logdq(new.theta)
      logacc.prob <- min(0, logacc.prob) # Note that 0 = log(1)

      if(log(runif(1)) < logacc.prob) {
        #Accept
        theta[i] <- new.theta
        accepted[i] <- TRUE
      } else {
        #Reject
        theta[i] <- theta[i - 1]
      }
    }

    print(summary(theta))

    print(input$thinning)
    print(n.iter)
    print(input$burnin)
    idx <- seq(input$thinning + 1, n.iter, by = input$thinning) 
    values$theta_sim <- theta[idx]
    values$accepted <- accepted[idx]


    values$LL <- as.vector(quantile(theta, 0.025))
    values$UL <- as.vector(quantile(theta, 0.975))


    # Density
    values$density <- density(theta)
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
    MH()
    tibble(
      "-" = c("Prior", "Posterior", "M-H posterior")
      ,
      Mean = c(
        mean_beta(input$a0, input$b0),
        mean_beta(input$a0 + sum(data$red), input$b0 + sum(data$MMs - data$red)),
        mean(values$theta_sim)
      ) %>%
        round(2)
      ,
      SD = c(
        sd_beta(input$a0, input$b0),
        sd_beta(input$a0 + sum(data$red), input$b0 + sum(data$MMs - data$red)),
        sd(values$theta_sim)
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
      "% accepted" = c(NA, NA, round(mean(values$accepted), 2))
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
      # Density plot from M-H
      geom_line(data = data.frame(x = values$density$x, y = values$density$y),
        aes(x = x, y = y))
  )


  output$trace <- renderPlot(

    ggplot(data.frame(iteration = 1:length(values$theta_sim), theta = values$theta_sim)) +
      geom_line(aes(x = iteration, y = theta)) +
      ggtitle("Trace")
  )

  output$dist_summaries <- renderTable({dist_summaries()})

}
