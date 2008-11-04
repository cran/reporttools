\name{tableDate}
\alias{tableDate}
\title{Display descriptive statistics for date variables}
\description{
Many data analyses start with a display of descriptive statistics of important variables. This function takes
a list of date variables and a possible grouping variable (such as e.g. treatment) and provides
a Latex table of descriptive statistics separately per group and jointly for all observations, per variable. 
}
\usage{
tableDate(vars, nams, group = NA, subset = NA, disp.cols = c("n", 
    "min", "q1", "median", "mean", "q3", "max", "na"), 
     col.tit = NA, print.pval = TRUE, cap = "", lab = "")
}
\arguments{
  \item{vars}{A list of date variables.}
  \item{nams}{A vector of strings, containing the names corresponding to the variables in \code{vars}. These are the names
  that appear in the Latex table.}
  \item{group}{Grouping variable, may be omitted.}
  \item{subset}{Only consider a subset of observations.}
  \item{disp.cols}{Specify which descriptive statistics should be displayed in the table.}
  \item{col.tit}{Specify titles of columns.}
  \item{print.pval}{If \code{print.pval == TRUE}, \eqn{p}-values of a Mann-Whitney or Kruskal-Wallis 
  test for a difference between groups are provided.}
  \item{cap}{The caption of the resulting Latex table.}
  \item{lab}{The label of the resulting Latex table.}
}
\value{Outputs the Latex table.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}}
\references{
Rufibach, K. (2008) 
reporttools: R-Functions to Generate Latex - Tables of Descriptive Statistics.
Preprint, University of Zurich.
}
\examples{
set.seed(1977)
diagnosis <- as.Date(round(runif(10, min = 35000, max = 40000)), 
    origin = "1899-12-30")
death <- as.Date(round(runif(10, min = 35000, max = 40000)), 
    origin = "1899-12-30")
vars <- list(diagnosis, death)
nams <- c("Diagnosis", "Death")
group <- sample(c(rep("A", 50), rep("B", 50)))
subset <- c(1:70, 91:100)
tableDate(vars, nams, group, subset, disp.cols = c("n", 
    "min", "q1", "median", "mean", "q3", "max", "na"), 
    col.tit = NA, print.pval = TRUE, cap = 
    "Table of date variables.", lab = "tab: descr stat date")
}
\keyword{manip}
\keyword{chron}

