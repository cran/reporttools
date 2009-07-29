\name{tableDate}
\alias{tableDate}
\title{Display descriptive statistics for date variables}
\description{
Many data analyses start with a display of descriptive statistics of important variables. This function takes
a data frame of date variables and possible grouping (such as e.g. treatment), weighting, and subset 
variables and provides a LaTeX table of descriptive statistics separately per group and jointly for all 
observations, per variable. 
}
\usage{
tableDate(vars, weights = NA, subset = NA, group = NA, 
     stats = c("n", "min", "q1", "median", "mean", "q3", "max", "na"), 
     col.tit = NA, print.pval = TRUE, cap = "", lab = "", font.size = 
     "footnotesize", longtable = TRUE, disp.cols = NA, nams = NA)
}
\arguments{
  \item{vars}{A data frame of date variables. See \code{nams} for an alternative
  way of specifying the variables to be displayed.}
  \item{weights}{Optional vector of weights of each observation.}
  \item{subset}{Optional logical vector, indicates subset of observations to be used.}
  \item{group}{Optional grouping variable.}
  \item{stats}{Specify which descriptive statistics should be displayed in the table, by either directly providing
  one or more of the default character strings (in arbitrary order).}
  \item{col.tit}{Specify titles of columns.}
  \item{print.pval}{If \code{print.pval == TRUE}, \eqn{p}-values of a Mann-Whitney or Kruskal-Wallis 
  test for a difference between groups are provided.}
  \item{cap}{The caption of the resulting LaTeX table.}
  \item{lab}{The label of the resulting LaTeX table.}
  \item{font.size}{Font size for the generated table in LaTeX.}
  \item{longtable}{If \code{TRUE}, function makes use of package \code{longtable} in LaTex to generate tables that 
  span more than one page. If \code{FALSE}, generates a table in \code{tabular} environment.}
  \item{disp.cols}{Only included for backward compatibility. Needs to be a vector of (some of) the default 
  statistics character strings if not equal to \code{NA}. From package version 1.0.2 use of \code{stats} is recommended.}  
  \item{nams}{A vector of strings, containing the names corresponding to the variables in \code{vars}, if \code{vars}
  is not a data frame but a list of variables. These are then the names that appear in the LaTeX table.
  This option is only kept for backward compatibility.}
}
\value{Outputs the LaTeX table.}
\note{If \code{longtable = TRUE} (which is the default), the function generates a table that may be more than one page 
long, you need to include the package longtable in the LaTeX source.

If a list of variables is given to \code{vars}, not all of these variables need to be of the same length. However, 
note the Warning below.
}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@ifspm.uzh.ch}, \cr \url{http://www.biostat.uzh.ch/aboutus/people/rufibach.html}}
\section{Warning}{If either one of the arguments \code{group}, \code{weights}, or \code{subset} is different from \code{NA}
and if \code{vars} is a list, then it is assumed that all variables 
in \code{vars} are of \emph{equal length}.
}
\references{
Rufibach, K. (2009) 
reporttools: R-Functions to Generate LaTeX Tables of Descriptive Statistics.
Journal of Statistical Software, Code Snippets, 31(1). \cr
\url{http://www.jstatsoft.org/v31/c01}.
}
\examples{
set.seed(1977)
diagnosis <- as.Date(round(runif(10, min = 35000, max = 40000)), 
    origin = "1899-12-30")
death <- as.Date(round(runif(10, min = 35000, max = 40000)), 
    origin = "1899-12-30")
vars <- data.frame(diagnosis, death)
group <- sample(c(rep("A", 5), rep("B", 5)))
tableDate(vars = vars, group = group, stats = c("n", "min", "median", "max", "na"), 
    cap = "Table of date variables.", lab = "tab: descr stat date")
    
## suppose we have weighted observations
weights <- c(2, 3, 1, 4, rep(1, 6))
subset <- 1:5
tableDate(vars = vars, weights = weights, subset = subset, 
    cap = "Table of date variables.", lab = "tab: descr stat date")

## For backward compatibility, disp.cols and nams are still working.
## If a list is given to vars, the variables can be of different length. 
## However, then weights, subset, and group must be set to NA (the default).
tableDate(vars = list(diagnosis, death), nams = c("Diagnosis", "Death"), 
    disp.cols = c("n", "na", "min", "max"), col.tit = c("observations", 
    "percentage", "cum. percentage"), print.pval = FALSE, cap = 
    "Table of date variables.", lab = "tab: descr stat date")    
}
\keyword{manip}
\keyword{chron}

