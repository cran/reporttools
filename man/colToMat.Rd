\name{colToMat}
\Rdversion{1.1}
\alias{colToMat}
\title{Break a n * p data frame in a data frame with ceiling(n / cols) rows and cols * p columns}
\description{Often, one does not want to span a data frame over several pages. This function
breaks a \eqn{n \times p} data frame in a data frame with \code{ceiling(n / cols)} rows and \code{cols * p} columns.}
\usage{colToMat(tab, cols)}
\arguments{
  \item{tab}{The data frame to be reformatted.}
  \item{cols}{Number of columns of the reformatted data.frame.}
}
\value{Returns the reformatted data frame.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@ifspm.uzh.ch}, \cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}}
\keyword{manip}
\keyword{character}
