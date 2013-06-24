\name{checkDateSuccession}
\alias{checkDateSuccession}
\title{Check whether dates in two vectors are elementwise consecutive}
\description{
Given two vectors \eqn{d_1} and \eqn{d_2} of date type, this function outputs all entries \eqn{d_{1j}} and \eqn{d_{2j}} 
such that \eqn{d_{1j} \ge d_{2j}}.
}
\usage{
checkDateSuccession(d1, d2, pat, names = NA, lab = "", typ = c("R", "tex")[2])
}
\arguments{
  \item{d1}{Supposedly earlier dates.}
  \item{d2}{Supposedly later dates.}
  \item{pat}{Corresponding list of patient (observation) numbers.}
  \item{names}{Names of date vectors, of length 3.}
  \item{lab}{Label of the generated latex table.}
  \item{typ}{Type of output.}
}
\value{
A latex table is output.
}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}
\examples{
set.seed(1977)
diagnosis <- as.Date(round(runif(10, min = 35000, max = 40000)), 
    origin = "1899-12-30")
death <- as.Date(round(runif(10, min = 35000, max = 40000)), 
    origin = "1899-12-30")

## check whether diagnosis was before death
checkDateSuccession(diagnosis, death, 1:10, names = c("Pat", 
    "diagnosis", "death"), lab = "tab: diag --> death") 

checkDateSuccession(diagnosis, death, 1:10, names = c("Pat", 
    "diagnosis", "death"), lab = "tab: diag --> death", typ = "R") 
}

\keyword{manip}
\keyword{chron}

