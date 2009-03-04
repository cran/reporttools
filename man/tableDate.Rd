\name{tableDate}
\alias{tableDate}
\title{Display descriptive statistics for date variables}
\description{
Many data analyses start with a display of descriptive statistics of important variables. This function takes
a list of date variables and a possible grouping variable (such as e.g. treatment) and provides
a LaTeX table of descriptive statistics separately per group and jointly for all observations, per variable. 
}
\usage{
tableDate(vars, nams, group = NA, subset = NA, stats = c("n", 
    "min", "q1", "median", "mean", "q3", "max", "na"), 
     col.tit = NA, print.pval = TRUE, cap = "", lab = "", disp.cols = NA)
}
\arguments{
  \item{vars}{A list of date variables.}
  \item{nams}{A vector of strings, containing the names corresponding to the variables in \code{vars}. These are the names
  that appear in the LaTeX table.}
  \item{group}{Grouping variable, may be omitted.}
  \item{subset}{Only consider a subset of observations.}
  \item{stats}{Specify which descriptive statistics should be displayed in the table, by either directly providing
  one or more of the default character strings (in arbitrary order).}
  \item{col.tit}{Specify titles of columns.}
  \item{print.pval}{If \code{print.pval == TRUE}, \eqn{p}-values of a Mann-Whitney or Kruskal-Wallis 
  test for a difference between groups are provided.}
  \item{cap}{The caption of the resulting LaTeX table.}
  \item{lab}{The label of the resulting LaTeX table.}
  \item{disp.cols}{Only included for backward compatibility. Needs to be a vector of (some of) the default 
  statistics character strings if not equal to \code{NA}. From package version 1.0.2 use of \code{stats} is recommended.}  
}
\value{Outputs the LaTeX table.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}}
\references{
Rufibach, K. (2009) 
reporttools: R-Functions to Generate LaTeX Tables of Descriptive Statistics.
Journal of Statistical Software, to appear.
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
tableDate(vars, nams, group, subset, stats = c("n", 
    "min", "q1", "median", "mean", "q3", "max", "na"), 
    col.tit = NA, print.pval = TRUE, cap = 
    "Table of date variables.", lab = "tab: descr stat date")
    
## for backward compatibility, disp.cols is still working
tableDate(vars, nams, group, subset, disp.cols = c("n", "na", "min", "max"), 
    col.tit = c("observations", "percentage", "cum. percentage"), 
    print.pval = TRUE, cap = "Table of date variables.", lab = "tab: descr stat date")    
}
\keyword{manip}
\keyword{chron}

