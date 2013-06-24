\name{attachPresAbs}
\alias{attachPresAbs}
\title{Attach levels absent and present to a 0-1 vector.}
\description{Attach levels "absent" and "present" to a 0-1 vector and turn it into a factor.}
\usage{attachPresAbs(v)}
\arguments{
\item{v}{Vector.}}
\value{Factor with the corresponding levels.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}
\examples{
set.seed(1977)
vec <- round(runif(10, 0, 1))
attachPresAbs(vec)
}

\keyword{manip}
\keyword{character}

