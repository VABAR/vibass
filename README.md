
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vibass

[<img src="http://vabar.es/images//widget_vibass7.jpg" width="300px"/>](http://vabar.es/events/vibass7/)

<!-- badges: start -->

[![R-CMD-check](https://github.com/VABAR/vibass/workflows/R-CMD-check/badge.svg)](https://github.com/VABAR/vibass/actions)
<!-- badges: end -->

Teaching materials for the [introductory course on Bayesian
inference](http://vabar.es/events/vibass7-intro/) at
[VIBASS](http://vabar.es/events/vibass7/)

## Installation

``` r
install.packages('vibass', repos = c('https://vabar.r-universe.dev', 'https://cloud.r-project.org'))
```

## Example

If installed with vignettes, the package contains the course practicals.
Otherwise, it points to the corresponding articles in the web site
<http://vabar.es/vibass/>.

Most importantly, it contains interactive apps associated with some of
the practicals

``` r
library(vibass)

## If installed with vignettes:
## Otherwise, read at: http://vabar.es/vibass/
browseVignettes('vibass')  # Index of all practicals
vignette('p1')  # Open practical

vibass_app()    # Lists available apps
vibass_app(1)   # Launch the interactive app for practical 1
```

![](man/figures/p1_app.png)
