\name{transformVarNames}
\alias{transformVarNames}
\title{Generate R-code assigning each variable in a data frame to its name}
\description{
This function generates a one-column matrix, containing strings of assignments of the variables in a 
data frame.}
\usage{transformVarNames(dat, name)}
\arguments{
  \item{dat}{Dataframe.}
  \item{name}{Name of data frame.}
}
\value{One-column matrix of strings containing the assignments.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}
\examples{
labpar1 <- rnorm(50)
labor.param2 <- rgamma(50, 2, 1)
dat <- data.frame(labpar1, labor.param2)
transformVarNames(dat, name = "dat")
}
\keyword{manip}
\keyword{character}

