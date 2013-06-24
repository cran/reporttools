% modified from 
% File src/library/base/man/format.pval.Rd
% Part of the R package, http://www.R-project.org

\name{formatPval}
\title{Format P Values}
\description{
  \code{formatPval} is intended for formatting \eqn{p}-values, and is based on
  the function \code{\link{format.pval}} in the base R-package.
}
\alias{formatPval}
\usage{
formatPval(pv, digits = max(1, getOption("digits") - 2),
           eps = 0.0001, na.form = "NA", scientific = FALSE,
           includeEquality=FALSE)
}
\arguments{
  \item{pv}{a numeric vector.}
  \item{digits}{how many significant digits are to be used.}
  \item{eps}{a numerical tolerance: see \sQuote{Details}.}
  \item{na.form}{character representation of \code{NA}s.}
  \item{scientific}{use scientific number format (not by default)}
  \item{includeEquality}{include equality signs in front of the large \eqn{p}-values? (not by default)} 
}
\value{A character vector.}
\details{
  \code{formatPval} is mainly an auxiliary function for the family of
  table functions, but can also be useful on its own. If a \eqn{p}-value is
  smaller than \code{eps}, we return just that it is smaller than the
  threshold but no longer the exact value. This function is more general
  than \code{\link{format.pval}} the behaviour of which can (almost) be
  obtained by using the options \code{eps = .Machine$double.eps} and 
  \code{scientific = TRUE}.
}
\examples{
## include equality signs?
formatPval(c(stats::runif(5), pi^-100, NA))
formatPval(c(stats::runif(5), pi^-100, NA), include=TRUE)

## try another eps argument
formatPval(c(0.1, 0.0001, 1e-7))
formatPval(c(0.1, 0.0001, 1e-7), eps=1e-7)

## only the white space can differ with the base function result:
(a <- formatPval(c(0.1, 0.0001, 1e-27),
                 eps = .Machine$double.eps, scientific = TRUE))
(b <- format.pval(c(0.1, 0.0001, 1e-27)))
all.equal(a, b)
}
\keyword{print}
