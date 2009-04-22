\name{tableNominal}
\alias{tableNominal}
\title{Display descriptive statistics for nominal variables}
\description{
Many data analyses start with a display of descriptive statistics of important variables. This function takes
a list of nominal variables and a possible grouping variable (such as e.g. treatment) and provides
a LaTeX table of descriptive statistics separately per group and jointly for all observations, per variable. 
}
\usage{tableNominal(vars, nams, weights = NA, subset = NA, 
    group = NA, miss.cat = NA, print.pval = c("none", "fisher", 
    "chi2")[1], vertical = TRUE, cap = "", lab = "")}
\arguments{
  \item{vars}{A list of nominal variables.}
  \item{nams}{A vector of strings, containing the names corresponding to the variables in \code{vars}. These are the names
  that appear in the LaTeX table.}
  \item{weights}{Optional vector of weights of each observation.}
  \item{subset}{Optional logical vector, indicates subset of observations to be used.}
  \item{group}{Optional grouping variable.}
  \item{miss.cat}{Vector specifying the factors in \code{vars} that should have their \code{NA}s transformed to a separate category.}
  \item{print.pval}{Give \eqn{p}-value of Fisher's exact or \eqn{\chi^2} test for a difference of distributions between 
  groups, if there is more than one group.}
  \item{vertical}{If \code{TRUE}, add vertical lines to the table, separating labels and groups, if applicable.}
  \item{cap}{The caption of the resulting LaTeX table.}
  \item{lab}{The label of the resulting LaTeX table.}
}
\value{Outputs the LaTeX table.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@ifspm.uzh.ch}, \cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}}
\section{Warning}{If either one of the arguments \code{group}, \code{weights}, or \code{subset} is different from \code{NA}, then it 
is assumed that all variables in \code{vars} are of \emph{equal length}.}
\references{
Rufibach, K. (2009) 
reporttools: R-Functions to Generate LaTeX Tables of Descriptive Statistics.
Journal of Statistical Software, to appear.
}
\examples{
set.seed(1977)
vars <- list(c(round(runif(90, 0, 3)), rep(NA, 10)), round(runif(100, 0, 2)))
nams <- c("Var1", "Var2")
group <- sample(c(rep("A", 50), rep("B", 50)))
weights <- c(rep(1, 70), rep(2, 20), rep(1, 10))
     
## only consider a subset of observations (use of 'subset' is no longer recommended, though!)
tableNominal(vars, nams, weights = weights, group = group, cap = "Table of nominal variables.", 
     lab = "tab: nominal")      

## only consider a subset of observations (use of 'subset' is no longer recommended, though!)
subset <- c(1:70, 91:100)
tableNominal(vars, nams, weights = weights, subset = subset, group = group, 
    cap = "Table of nominal variables.", lab = "tab: nominal")     
}
\keyword{manip}
\keyword{character}


