\name{NAtoZero}
\alias{NAtoZero}
\title{Change NAs in a vector into a given value.}
\description{Extract all the missing values in a vector and turn them into a given value.}
\usage{NAtoZero(v, value = 0)}
\arguments{
\item{v}{Vector.}
\item{value}{Value to be given to the missing valus.}}
\value{Updated vector.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@ifspm.uzh.ch}, \cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}}
\examples{
set.seed(1977)
vec <- sample(c(round(runif(10, 1, 3)), rep(NA, 10)))
NAtoZero(vec)
}

\keyword{manip}
\keyword{character}

