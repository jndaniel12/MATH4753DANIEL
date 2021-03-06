% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myncurve.R
\name{myncurve}
\alias{myncurve}
\title{A function to display a normal curve.}
\usage{
myncurve(a, mu, sigma)
}
\arguments{
\item{a}{The probability that X will be less than or equal to.}

\item{mu}{The mean.}

\item{sigma}{The standard deviation.}
}
\value{
Makes a curve, shades the area from negative infinity to a,
calculates the probability that X <= a, P(X <= a), prints the calculated
probability to the console.
}
\description{
A function to display a normal curve.
}
\examples{
\dontrun{myncurve(3, 10, 5)}
}
