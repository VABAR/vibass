
options(shiny.usecairo = FALSE)

cols <- transform(
  data.frame(
    target = c("prior", "likelihood", "posterior", "predictive"),
    name = c("dodgerblue", "darkorange", "darkgreen", "purple")
  ),
  hex = colorspace::hex(
    do.call(
      colorspace::RGB,
      setNames(
        as.data.frame(
          t(col2rgb(name)/255)
        ),
        c("R", "G", "B")
      )
    )
  )
)

plot_style <- list(
  # labs(x = expression(theta), y = NULL, color = NULL),
  scale_colour_manual(
    values = setNames(cols$name, cols$target)
  ),
  theme_minimal(),
  theme(
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
)

theta_values <- data.frame(
  x = seq(0.01, .99, length = 101)
)

qbbinom <- function(p, n, a, b) {
  # n <- 10; a <- 5; b <- 8
  stopifnot(
    all(between(p, 0, 1))
  )
  cdf <- pbbinom(0:n, n, a, b)

  q <- apply(
    vapply(p, '<=', rep(T, length(cdf)), cdf),
    2,
    function(.) head(which(.), 1)
  ) - 1

  return(q)
}


mean_beta <- function(a, b) a / (a + b)
sd_beta <- function(a, b) sqrt( a * b / ( (a + b)**2 * (a + b + 1) ) )

## TODO
## - Add button "Take snapshot" and plot the curves greyed out in the
## background. Also need a button "Reset", to clear out the memory.
## + Let a0 and b0 go up to 100 (and eventually put values)
## + Add some introductory text and equations
## + Add a graphical representation of the 95% CrI
## + Add a table with Mean, SD and 95 % CrI for the prior, posterior and
## predictive
## + Put into a R package
