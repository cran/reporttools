\name{attachPresAbs}
\alias{attachPresAbs}
\title{Attach levels absent and present to a 0-1 vector.}
\description{Attach levels "absent" and "present" to a 0-1 vector and turn it into a factor.}
\usage{attachPresAbs(v)}
\arguments{
\item{v}{Vector.}}
\value{Factor with the corresponding levels.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@ifspm.uzh.ch}, \cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}}
\examples{
set.seed(1977)
vec <- round(runif(10, 0, 1))
attachPresAbs(vec)
}

\keyword{manip}
\keyword{character}

