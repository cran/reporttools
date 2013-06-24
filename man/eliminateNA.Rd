\name{eliminateNA}
\alias{eliminateNA}
\title{Eliminate all observations with at least one NA in a data frame}
\description{
Generates two matrices: One with complete observations and one with all observations containing at least
one missing value.
}
\usage{eliminateNA(dat)}
\arguments{\item{dat}{Dataframe with observations in rows.}}
\value{
  \item{complete}{Dataframe containing complete observations.}
  \item{incomplete}{Dataframe containing observations with at least one \code{NA}.}
}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}
\seealso{\code{\link{complete.cases}}}
\examples{
pat <- 1:10; var1 <- rnorm(10); var2 <- factor(round(rgamma(10, 2, 1)))
dat <- data.frame(cbind(pat, var1, var2))
dat[c(2, 8), 3] <- NA
eliminateNA(dat)
}

\keyword{manip}
\keyword{character}

