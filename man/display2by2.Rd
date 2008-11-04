\name{display2by2}
\alias{display2by2}
\title{Function to display a 2 x 2 table including col- and row-names and totals}
\description{
Generate a Latex table of a 2 x 2 table that contains not only the cell frequencies, but also
pre-specified row- and col-names as well as totals of rows and cols.}
\usage{
display2by2(v1, v2, names = c("v1", "v2"), cap = "", lab = "", 
    row.nam = NA, col.nam = NA)
}
\arguments{
  \item{v1}{Vector with entries in \eqn{\{0, 1\}}.}
  \item{v2}{Vector with entries in \eqn{\{0, 1\}}.}
  \item{names}{Names of the vectors under consideration.}
  \item{cap}{Caption of the Latex table to be generated.}
  \item{lab}{Label of the Latex table to be generated.}
  \item{row.nam}{Labels of \code{v1} to be given as row names.}
  \item{col.nam}{Labels of \code{v2} to be given as column names.}
}
\value{
Returns a Latex 2 x 2 table.
}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}}
\examples{
set.seed(1977)
v1 <- round(runif(20))
v2 <- round(runif(20))
display2by2(v1, v2, names = c("v1", "v2"), cap = "", lab = "", row.nam = NA, 
    col.nam = NA) 
}

\keyword{manip}
\keyword{character}

