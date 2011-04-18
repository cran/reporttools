\name{attachYesNo}
\alias{attachYesNo}
\title{Attach levels no and yes to a 0-1 vector.}
\description{Attach levels "no" and "yes" to a 0-1 vector and turn it into a factor.}
\usage{attachYesNo(v)}
\arguments{
\item{v}{Vector.}}
\value{Factor with the corresponding levels.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@ifspm.uzh.ch}, \cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}}
\examples{
set.seed(1977)
vec <- round(runif(10, 0, 1))
attachYesNo(vec)
}

\keyword{manip}
\keyword{character}

