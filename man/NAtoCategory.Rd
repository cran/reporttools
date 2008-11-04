\name{NAtoCategory}
\alias{NAtoCategory}
\title{Change NAs in a factor into a category}
\description{Extract all the missing values in a factor variable and turn them into a separate category.}
\usage{NAtoCategory(fact, label = "missing")}
\arguments{
\item{fact}{Factor variable.}
\item{label}{Label to be given to the missing valus.}}
\value{Updated factor variable.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}}
\examples{
set.seed(1977)
fact <- factor(sample(c(round(runif(10, 1, 3)), rep(NA, 10))), levels = 1:3, 
    labels = c("no", "maybe", "yes"))
NAtoCategory(fact)
}

\keyword{manip}
\keyword{character}

