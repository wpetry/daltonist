% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_accessible.R
\name{check_accessible}
\alias{check_accessible}
\title{Check whether a color palette is accessible to colorblind folks}
\usage{
check_accessible(cols, cb = c("all", "deutan", "protan", "tritan"),
  severity = 1)
}
\arguments{
\item{cols}{a character vector of hexadecimal color codes}

\item{cb}{type of colorblindness to simulate}

\item{severity}{intensity of colorblindness [0-1]}
}
\value{
an object of class cbAccess
}
\description{
Check whether a color palette is accessible to colorblind folks
}
\examples{
check_accessible(cols = c("#ffffff", "#394cda"), cb = "all", severity = 1)
}
