\name{displayKbyC}
\alias{displayKbyC}
\title{Function to display a K x C frequency table including col- and row-names and totals}
\description{
Generate a LaTeX table of a \eqn{K \times C} frequency table that contains not only the cell frequencies, but also
pre-specified row- and col-names as well as totals of rows and cols.}
\usage{
displayKbyC(v1, v2, percentage = c("none", "row", "col", 
    "total")[1], names = c("v1", "v2"), cap = "", 
    lab = "", row.nam = NA, col.nam = NA)
}
\arguments{
  \item{v1}{Vector with \code{integer} entries.}
  \item{v2}{Vector with \code{integer} entries.}
  \item{percentage}{Add percentages with respect to row, column, or table total.}
  \item{names}{Names of the vectors under consideration.}
  \item{cap}{Caption of the LaTeX table to be generated.}
  \item{lab}{Label of the LaTeX table to be generated.}
  \item{row.nam}{Labels of \code{v1} to be given as row names.}
  \item{col.nam}{Labels of \code{v2} to be given as column names.}
}
\value{Returns a LaTeX K x C table.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}
\examples{
set.seed(1977)
v1 <- round(runif(20, 0, 3))
v2 <- round(runif(20, 0, 5))
displayKbyC(v1, v2, percentage = "row", names = c("v1", "v2"), 
    cap = "", lab = "", row.nam = NA, col.nam = NA) 
}

\keyword{manip}
\keyword{character}

