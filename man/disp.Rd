\name{disp}
\alias{disp}
\title{Display vectors of numbers, especially targeted to vectors of p-values}
\description{This function serves to display numbers in plain text, using a given number of digits after the comma.}
\usage{disp(n, d1 = 2, d2 = 1)}
\arguments{
  \item{n}{Vector of real numbers to be displayed.}
  \item{d1}{Number of digits numbers are basically rounded to.}
  \item{d2}{If numbers in \eqn{n} are smaller than \eqn{10^{-\code{d1}}}, then \code{d2} significant digits are given.}
}
\value{\item{t}{A vector of character strings containing the input number \code{n} formatted as specified by \code{d1}
and \code{d2}.}}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}
\examples{
r <- c(0.23445, 0.000089)
disp(r)}

\keyword{manip}
\keyword{character}
