\name{vectorToString}
\Rdversion{1.1}
\alias{vectorToString}
\title{Generate a string of the elements of a vector}
\description{A string concatenating the elements of a vector is generated.}
\usage{vectorToString(v, symb = " / ")}
\arguments{
  \item{v}{Vector.}
  \item{symb}{Symbol that separates elements in string.}
}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@ifspm.uzh.ch}, \cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}}
\examples{
v <- 1:5
vectorToString(v, symb = ":")
}
\keyword{manip}
\keyword{character}
