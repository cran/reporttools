\name{disp}
\alias{disp}
\title{Display numbers in text}
\description{This function serves to display numbers in plain text, using a given number of digits after the comma.}
\usage{disp(n, d = 2)}
\arguments{
  \item{n}{(Real) Number to be displayed.}
  \item{d}{Number of digits after the comma.}
}
\value{\item{t}{A character string containing the input number \eqn{n} formatted as specified by \eqn{d}.}}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}}
\examples{
r <- pi
disp(r)}

\keyword{manip}
\keyword{character}
