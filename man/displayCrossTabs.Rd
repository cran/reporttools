\name{displayCrossTabs}
\alias{displayCrossTabs}
\title{Function to display a set of K x C frequency tables, including p-value}
\description{
For each column of a dataframe, generate a LaTeX table against a given variable using \code{displayKbyC} and add a suitable \eqn{p}-value:
If the expected frequencies are all \eqn{>5} then a \eqn{\chi^2}-test is computed, otherwise Fisher's exact test.}
\usage{
displayCrossTabs(vars, v0, nam0, lab0, 
    percentage = c("none", "row", "col", "total")[1])
}
\arguments{
  \item{vars}{Dataframe of nominal variables.}
  \item{v0}{Nominal variable to tabulate all columns of \code{vars} against.}
  \item{nam0}{Name of \code{v0}.}
  \item{lab0}{Initial string for table label. The column number of \code{vars} will be added, so that each table has a unique label.}
  \item{percentage}{Add percentages with respect to row, column, or table total.}
}
\value{Displays LaTeX K x C tables and returns a list containing all the information.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}
\examples{
set.seed(1977)
v0 <- round(runif(20, 0, 5))
v1 <- round(runif(20, 0, 3))
v2 <- round(runif(20, 0, 4))
displayCrossTabs(vars = data.frame(v1, v2), v0, nam0 = "v0", lab0 = "Q1") 
}

\keyword{manip}
\keyword{character}

