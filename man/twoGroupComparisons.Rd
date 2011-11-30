\name{twoGroupComparisons}
\alias{twoGroupComparisons}
\title{Compute a table with analysis of two groups comparisons}
\description{
For each column of a dataframe, generate a row in a resulting table that contains basic descriptive statistics, effect size, \eqn{p}-value, 
and confidence intervals for a two group comparions, where the grouping variable is separately given.}
\usage{twoGroupComparisons(vars, v0, conf.level = 0.95, paired = FALSE)}
\arguments{
  \item{vars}{Dataframe of continuous variables.}
  \item{v0}{Binary variable that builds the two groups.}
  \item{conf.level}{Confidence level used in computation of confidence intervals.}
  \item{paired}{Logical, indicate whether comparisons are paired or not.}
}
\value{A list consisting of the following elements:
\item{\code{raw}}{Matrix that contains the above as raw numbers.}
\item{\code{formatted}}{The same table where numbers are formatted and confidence intervals are given as character string.}
}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}
\examples{
set.seed(1977)
v0 <- round(runif(200, 0, 1))
v1 <- rnorm(200)
v2 <- rgamma(200, 2, 1)
twoGroupComparisons(vars = data.frame(v1, v2), v0) 
}

\keyword{manip}
\keyword{character}

