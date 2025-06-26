
options(shiny.usecairo = FALSE)

fig_transformations <-
  data.frame(
    x = seq(0.01, .99, length = 101)
  ) %>%
  mutate(
    odds = x/(1-x),
    logodds = log(odds)
  ) %>%
  pivot_longer(
    cols = -x,
    names_to = "transf",
    values_to = "y"
  ) %>%
  ggplot(aes(x, y)) +
  geom_line(aes(group = transf)) +
  geom_text(
    data = data.frame(
      x = c(.25, .75),
      y = c(1, 0),
      l = c("odds", "log-odds")
    ),
    aes(label = l)
  ) +
  coord_cartesian(
    ylim = c(-1, 1) * 5
  ) +
  theme_minimal() +
  theme(
    # axis.text.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    x = "Probability",
    y = NULL
  )


mm_cols <-
  c(
    red = "#b11224",
    yellow = "#fff200",
    blue = "#2f9fd7",
    green = "#31ac55",
    orange = "#f26f22",
    brown = "#603a34"
  )

cols <- data.frame(
  target = c("prior", "likelihood", "posterior", "predictive"),
  name = c("dodgerblue", "darkorange", "darkgreen", "purple"),
  hex = c("#60C6FF", "#FFC400", "#00A800", "#D063F8"),
  stringsAsFactors = FALSE
)

## Hex codes from named colours
# colorspace::hex(
#   do.call(
#     colorspace::RGB,
#     setNames(
#       as.data.frame(
#         t(col2rgb(cols$name)/255)
#       ),
#       c("R", "G", "B")
#     )
#   )
# )

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


## TODO
## - Add button "Take snapshot" and plot the curves greyed out in the
## background. Also need a button "Reset", to clear out the memory.
## + Let a0 and b0 go up to 100 (and eventually put values)
## + Put into a R package
