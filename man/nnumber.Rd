% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/src.R
\name{nnumber}
\alias{nnumber}
\title{neat representation of numbers}
\usage{
nnumber(number, digits = 1)
}
\arguments{
\item{number}{an integer or double}

\item{digits}{number of digits to round-off. Default value is 1.}
}
\value{
String representation of numbers with suffix denoting K for thousands,
Mn for millions, Bn for billions, Tn for trillions. A number lower than thousand is
represented as it is.
}
\description{
neat representation of numbers
}
\examples{
x <- c(10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000)
nnumber(x)
nnumber(123456789.123456, digits = 1)
}
