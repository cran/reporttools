\name{varNamesToChar}
\alias{varNamesToChar}
\title{Split a character string into variable names}
\description{Transform a given string of variable names, separated by ", ", into a vector of corresponding variable names.}
\usage{
varNamesToChar(varnam)
}
\arguments{\item{varnam}{Character string, where variable names are separated by commas.}}
\value{Vector of variable names.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}}
\examples{
nams <- "var1, var2, var3"
varNamesToChar(nams)
}

\keyword{manip}
\keyword{character}

