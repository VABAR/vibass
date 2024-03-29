
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vibass

[<img src="http://vabar.es/images//widget_vibass6.png" width="300px"/>](http://vabar.es/events/vibass6/)

<!-- badges: start -->

[![R-CMD-check](https://github.com/VABAR/vibass/workflows/R-CMD-check/badge.svg)](https://github.com/VABAR/vibass/actions)
<!-- badges: end -->

Teaching materials for the [introductory course on Bayesian
inference](http://vabar.es/events/vibass6-intro/) at
[VIBASS](http://vabar.es/events/vibass6/)

## Installation

``` r
install.packages('vibass', repos = c('https://vabar.r-universe.dev', 'https://cloud.r-project.org'))
```

## Example

The package contains the course practicals and associated interactive
apps

``` r
library(vibass)
browseVignettes('vibass')  # Index of all practicals
vignette('p1')  # Open practical 1
vibass_app(1)   # Launch the interactive app for practical 1
```

![](man/figures/p1_app.png)
