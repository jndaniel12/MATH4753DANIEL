% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myclt.R
\name{myclt}
\alias{myclt}
\title{A function utilizing central limit theorem to produce a histogram
for a uniform distribution.}
\usage{
myclt(n, iter, a = 0, b = 5)
}
\arguments{
\item{n}{the sample size}

\item{iter}{the number of iterations}

\item{a}{the lowest value of x (for a Uniform)}

\item{b}{the highest value of x(for a Uniform)}
}
\value{
a histogram of the distribution of sums
}
\description{
A function utilizing central limit theorem to produce a histogram
for a uniform distribution.
}
\examples{
\dontrun{myclt(n=20, iter=10000, a=3, b=6)}
}
