\name{correctVarNames}
\Rdversion{1.1}
\alias{correctVarNames}
\title{Modify all entries in a data frame such that xtable has no problems displaying them}
\description{Replace all relevant characters in the entries and row- and colnames of a data frame
such that xtable does not complain displaying them.}
\usage{correctVarNames(tab, rowcol = TRUE, cols = NA)}
\arguments{
  \item{tab}{The data frame to be formatted.}
  \item{rowcol}{If \code{TRUE}, row- and colnames are reformatted.}
  \item{cols}{Provide a vector of column indices of columns to be reformatted. If \code{NA}, all columns are reformatted.}
}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@ifspm.uzh.ch}, \cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}}
\keyword{manip}
\keyword{character}

