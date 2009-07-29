\name{tableNominal}
\alias{tableNominal}
\title{Display descriptive statistics for nominal variables}
\description{
Many data analyses start with a display of descriptive statistics of important variables. This function takes
a data frame of nominal variables and possible grouping (such as e.g. treatment), weighting, and subset 
variables and provides a LaTeX table of descriptive statistics separately per group and jointly for all 
observations, per variable. 
}
\usage{tableNominal(vars, weights = NA, subset = NA, 
    group = NA, miss.cat = NA, print.pval = c("none", "fisher", 
    "chi2")[1], vertical = TRUE, cap = "", lab = "", font.size = 
    "footnotesize", longtable = TRUE, nams = NA)}
\arguments{
  \item{vars}{A data frame of nominal variables. See \code{nams} for an alternative
  way of specifying the variables to be displayed.}
  \item{weights}{Optional vector of weights of each observation.}
  \item{subset}{Optional logical vector, indicates subset of observations to be used.}
  \item{group}{Optional grouping variable.}
  \item{miss.cat}{Vector specifying the factors in \code{vars} that should have their \code{NA}s transformed to a separate category.}
  \item{print.pval}{Add \eqn{p}-values of Fisher's exact or \eqn{\chi^2} test for a difference of distributions between 
  groups to the table, if there is more than one group.}
  \item{vertical}{If \code{TRUE}, add vertical lines to the table, separating labels and groups, if applicable.}
  \item{cap}{The caption of the resulting LaTeX table.}
  \item{lab}{The label of the resulting LaTeX table.}
  \item{font.size}{Font size for the generated table in LaTeX.}
  \item{longtable}{If \code{TRUE}, function makes use of package \code{longtable} in LaTex to generate tables that 
  span more than one page. If \code{FALSE}, generates a table in \code{tabular} environment.}
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
data(CO2)
vars <- CO2[, 1:2]
group <- CO2[, "Treatment"]
weights <- c(rep(1, 60), rep(0, 10), rep(2, 14))
     
## only consider a subset of observations
tableNominal(vars = vars, weights = weights, group = group, cap = 
    "Table of nominal variables.", lab = "tab: nominal")      

## only consider a subset of observations
subset <- c(1:50, 60:70)
tableNominal(vars = vars, weights = weights, subset = subset, group = group, 
    cap = "Table of nominal variables.", lab = "tab: nominal")     
}
\keyword{manip}
\keyword{character}


