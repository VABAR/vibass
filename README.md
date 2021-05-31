
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vibass

[<img src="http://vabar.es/images//widget_vibass4.png" width="300px"/>](http://vabar.es/events/vibass4/)

<!-- badges: start -->
<!-- badges: end -->

Teaching materials for the [introductory course on Bayesian
inference](http://vabar.es/events/vibass4-intro/) at
[VIBASS](http://vabar.es/events/vibass4/)

## Installation

``` r
# install.packages("remotes")
remotes::install_github("VABAR/vibass")
```

## Example

The package contains the course practicals and associated interactive
apps

``` r
library(vibass)
vignette("p1", package = "vibass")  # Open practical 1
vibass_app(1)  # Launch the interactive app for practical 1
```

![](man/figures/p1_app.png)
