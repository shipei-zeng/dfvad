% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/value_decom.R
\name{roll_prod}
\alias{roll_prod}
\title{Converting Growth Values to Level Values}
\usage{
roll_prod(x)
}
\arguments{
\item{x}{A vector with growth values.}
}
\value{
A vector of level values.
}
\description{
\code{roll_prod()} converts growth values to level values for a vector.
}
\examples{
table1 <- value_decom(c("h2","x2"), c("w2","u2"), "y2", "p2", "year", mining)[[1]]
roll_prod(table1[, "TFPG"])
}
