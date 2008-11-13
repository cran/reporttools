\name{tableNominal}
\alias{tableNominal}
\title{Display descriptive statistics for nominal variables}
\description{
Many data analyses start with a display of descriptive statistics of important variables. This function takes
a list of nominal variables and a possible grouping variable (such as e.g. treatment) and provides
a Latex table of descriptive statistics separately per group and jointly for all observations, per variable. 
}
\usage{tableNominal(vars, nams, group, subset = NA, miss.cat = NA, 
    print.pval = c("none", "fisher", "chi2")[1], vertical = TRUE, 
    cap = "", lab = "")}
\arguments{
  \item{vars}{A list of nominal variables.}
  \item{nams}{A vector of strings, containing the names corresponding to the variables in \code{vars}. These are the names
  that appear in the Latex table.}
  \item{group}{Grouping variable, may be omitted.}
  \item{subset}{Only consider a subset of observations.}
  \item{miss.cat}{Vector specifying the factors in \code{vars} that should have their \code{NA}s transformed to a separate category.}
  \item{print.pval}{Give \eqn{p}-value of Fisher's exact or \eqn{\chi^2} test for a difference of distributions between 
  groups, if there is more than one group.}
  \item{vertical}{If \code{TRUE}, add vertical lines to the table, separating labels and groups, if applicable.}
  \item{cap}{The caption of the resulting Latex table.}
  \item{lab}{The label of the resulting Latex table.}
}
\value{Outputs the Latex table.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}}
\references{
Rufibach, K. (2008) 
reporttools: R-Functions to Generate LaTeX Tables of Descriptive Statistics.
Preprint, University of Zurich.
}
\examples{
set.seed(1977)
vars <- list(c(round(runif(90, 0, 3)), NA), round(runif(100, 0, 2)))
nams <- c("Var1", "Var2")
group <- sample(c(rep("A", 50), rep("B", 50)))
subset <- c(1:70, 91:100)
tableNominal(vars, nams, group, subset, cap = "Table of nominal variables.", 
     lab = "tab: nominal")
}
\keyword{manip}
\keyword{character}


