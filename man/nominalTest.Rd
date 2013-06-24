\name{nominalTest}
\alias{nominalTest}
\title{Compute Chi square or Fisher's exact test}
\description{Depending on the value of the smallest expected count compute either a \eqn{\chi^2} or Fisher's exact test.}
\usage{nominalTest(tab, limit.exp = 5)}
\arguments{
    \item{tab}{Frequency table, received by applying \code{table()} to two nominal variables.}  
    \item{limit.exp}{If the smallest expected count is at most \code{limit.exp} the \eqn{p}-value of a Fisher test is returned.
    Otherwise, a \eqn{\chi^2} test is computed.}
}
\value{A list containing:
\item{p}{The computed \eqn{p}-value.}
\item{test}{A string indicating the test that was used.}
}
\examples{
v1 <- as.factor(round(runif(40, 0, 3)))
v2 <- as.factor(round(runif(40, 2, 3)))
tab <- table(v1, v2)
nominalTest(tab)
}
\keyword{htest}



