% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rstudio-toggle_subsections.R
\name{getFormalValues}
\alias{getFormalValues}
\title{Title}
\usage{
getFormalValues(functionText)
}
\arguments{
\item{functionText}{}
}
\description{
Title
}
\examples{
a <- 1 # won't be found
b <- 2 # won't be found
functionText <- c("fun1<- function(",
          "a,",
          "b = c(1,a = 2),",
          "d = 2) {",
          "a^2",
          "if (a ==2) {",
          "b}",
          "}")
functionText <- c("fun <- function(a, b = 1, x = c(\"option1\", \"option2\"), y = NULL) {", 
"  # do stuff", "}")
}
