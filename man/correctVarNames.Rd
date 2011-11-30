\name{correctVarNames}
\Rdversion{1.1}
\alias{correctVarNames}
\title{Modify all entries in a data frame such that xtable has no problems displaying them}
\description{Replace all relevant characters in the entries and row- and colnames of a data frame
such that \code{xtable} does not complain displaying them.}
\usage{correctVarNames(tab, rowcol = TRUE, cols = 1:ncol(tab))}
\arguments{
  \item{tab}{The data frame to be formatted.}
  \item{rowcol}{If \code{TRUE}, row- and colnames are reformatted.}
  \item{cols}{Provide a vector of column indices of columns whose entries are to be reformatted. If \code{NA}, none of the entries of the initial data frame is formatted.}
}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}
\keyword{manip}
\keyword{character}

