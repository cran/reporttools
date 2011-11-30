\name{displayCI}
\alias{displayCI}
\title{Generate strings of a confidence interval from a matrix}
\description{This function serves to display a confidence interval in plain text, taking a vector of length 2
or a \eqn{d \times 2}-matrix containing the confidence limits and given number of digits after the comma.
A unit can be additionally supplied.}
\usage{displayCI(ci, digit = 2, unit = "")}
\arguments{
  \item{ci}{Vector of length 2 or matrix of size \eqn{d \times 2}, containing the confidence limits.}
  \item{digit}{Number of digits after the comma.}
  \item{unit}{Character string denoting a unit of measurement.}
}
\value{A character string to be inserted in plain text.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}
\examples{
a <- 0.05
k <- qnorm(p = 1 - a / 2)
x <- 50
n <- 100
wilson.ci <- (x + k ^ 2 / 2) / (n + k ^ 2) + c(-1, 1) * (k * n ^ 0.5) / 
    (n + k ^ 2) * sqrt(x / n * (1 - x / n) + k ^ 2 / (4 * n))
displayCI(wilson.ci)
displayCI(wilson.ci, digit = 1, unit = "cm")
}

\keyword{manip}
\keyword{character}
