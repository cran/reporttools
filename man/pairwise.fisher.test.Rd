\name{pairwise.fisher.test}
\alias{pairwise.fisher.test}
\title{Pairwise Fisher's exact test}
\description{Similar to \code{\link{pairwise.wilcox.test}} and \code{\link{pairwise.t.test}}, calculate pairwise comparisons of a nominal variable between group levels with corrections for multiple testing.}
\usage{pairwise.fisher.test(x, g, p.adjust.method, ...)}
\arguments{
  \item{x}{Response vector, nominal (or ordinal).}
  \item{g}{Grouping vector or factor.}
  \item{p.adjust.method}{Method for adjusting \eqn{p}-values (see \code{\link{p.adjust}}).}
  \item{...}{Additional arguments to pass to \code{\link{fisher.test}}.}
}
\value{Object of class "pairwise.htest"}

\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}

\seealso{\code{\link{fisher.test}}, \code{\link{p.adjust}}, \code{\link{pairwise.wilcox.test}}, \code{\link{pairwise.t.test}}}

\examples{
set.seed(1977)
x <- factor(abs(round(rnorm(99, 0, 1))))
g <- factor(round(runif(99, 0, 2)))
pairwise.fisher.test(x, g, p.adjust.method = "holm")
}

\keyword{dplot}
\keyword{aplot}
