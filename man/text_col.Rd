% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{text_col}
\alias{text_col}
\title{Return text in a contrasting colour}
\usage{
text_col(x)
}
\arguments{
\item{x}{Character. Text to print.}
}
\description{
Overcome default colouring schemes (e.g. for packageStartupMessages())
and make sure some text is printed in a contrasting colour depending
on the theme (dark or light) in RStudio.
}
\details{
Uses \code{cli} for printing white text in dark themes or black text
on light themes in RStudio.
Borrowed from package \code{tidyverse} (https://github.com/tidyverse/tidyverse/blob/72af810106d7249c905d6b0f5b8b42dc33e6ac21/R/utils.R)
}
\examples{
vibass:::text_col("Hello world")
message("Hello world")
message(vibass:::text_col("Hello world"))
}
