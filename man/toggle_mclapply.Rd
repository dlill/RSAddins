% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rstudio-toggle_subsections.R
\name{toggle_mclapply}
\alias{toggle_mclapply}
\title{Insert the arguments of a loop into the script}
\usage{
toggle_mclapply()
}
\value{
NULL. Modifies the document
}
\description{
for (a in 1:3) gets turned into
}
\details{
a <- (1:3)[\link{1}]
for (a in 1:3)

This is handy for developing and debugging a loop
}
\author{
Daniel Lill (daniel.lill@intiquan.com)
}
