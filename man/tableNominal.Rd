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
    "chi2"), pval.bound = 10^-4, fisher.B = 2000, vertical = TRUE, 
    cap = "", lab = "", col.tit.font = c("bf", "", "sf", "it", "rm"), 
    font.size = "footnotesize", longtable = TRUE, nams = NA, 
    cumsum = TRUE, ...)}
\arguments{
  \item{vars}{A data frame of nominal variables. See \code{nams} for an alternative
  way of specifying the variables to be displayed.}
  \item{weights}{Optional vector of weights of each observation.}
  \item{subset}{Optional logical vector, indicates subset of observations to be used.}
  \item{group}{Optional grouping variable.}
  \item{miss.cat}{Vector specifying the factors in \code{vars} that should have their \code{NA}s transformed to a separate category.}
  \item{print.pval}{Add \eqn{p}-values of Fisher's exact or \eqn{\chi^2} test for a difference of distributions between 
  groups to the table, if there is more than one group. Note that by default, any missing values are removed for computation of \eqn{p}-values. If 
  missings should be considered a separate level, define the input variables accordingly.}
  \item{pval.bound}{\eqn{p}-values below \code{pval.bound} are formatted as \code{< pval.bound}.}
  \item{fisher.B}{Number of simulations to compute \eqn{p}-value for Fisher's exact test. Note that in the function
  \code{fisher.test} the option \code{simulate.p.value} is set to
  \code{TRUE}, unless \code{fisher.B == Inf} which asks for the exact computation.}
  \item{vertical}{If \code{TRUE}, add vertical lines to the table, separating labels and groups, if applicable.}
  \item{cap}{The caption of the resulting LaTeX table.}
  \item{lab}{The label of the resulting LaTeX table.}
  \item{col.tit.font}{Choose the font for the column titles here (default: boldface).}
  \item{font.size}{Font size for the generated table in LaTeX.}
  \item{longtable}{If \code{TRUE}, function makes use of package \code{longtable} in LaTex to generate tables that 
  span more than one page. If \code{FALSE}, generates a table in \code{tabular} environment.}
  \item{nams}{A vector of strings, containing the names corresponding to the variables in \code{vars}, if \code{vars}
  is not a data frame but a list of variables. These are then the names that appear in the LaTeX table.
  This option is only kept for backward compatibility.}
  \item{cumsum}{If \code{TRUE}, the cumulative sums of the percentages
    are included for every level of the grouping variable.}
  \item{...}{Arguments pass through to \code{print.xtable}.}    
}
\value{Outputs the LaTeX table.}
\note{If \code{longtable = TRUE} (which is the default), the function generates a table that may be more than one page 
long, you need to include the package longtable in the LaTeX source.

If a list of variables is given to \code{vars}, not all of these variables need to be of the same length. However, 
note the Warning above.
}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}
\section{Warning}{If either one of the arguments \code{group}, \code{weights}, or \code{subset} is different from \code{NA}
and if \code{vars} is a list, then it is assumed that all variables 
in \code{vars} are of \emph{equal length}.
}
\references{
Rufibach, K. (2009) 
reporttools: R-Functions to Generate LaTeX Tables of Descriptive Statistics.
Journal of Statistical Software, Code Snippets, 31(1). \cr
\doi{https://doi.org/10.18637/jss.v031.c01}.
}
\examples{
data(CO2)
vars <- CO2[, 1:2]
group <- CO2[, "Treatment"]
weights <- c(rep(1, 60), rep(0, 10), rep(2, 14))
     
## first all observations
tableNominal(vars = vars, weights = weights, group = group, cap = 
    "Table of nominal variables.", lab = "tab: nominal")

## do not include cumulative percentages
tableNominal(vars = vars, weights = weights, group = group, cap = 
    "Table of nominal variables.", lab = "tab: nominal", cumsum = FALSE)

## but include p-value for Fisher's exact test
tableNominal(vars = vars, weights = weights, group = group, cap = 
    "Table of nominal variables.", lab = "tab: nominal",
    print.pval = "fisher", cumsum = FALSE)     

## Fisher's exact test without simulated p-value
tableNominal(vars = vars, weights = weights, group = group, cap = 
    "Table of nominal variables.", lab = "tab: nominal",
    print.pval = "fisher", fisher.B = Inf, cumsum = FALSE)     

## then only consider a subset of observations
subset <- c(1:50, 60:70)
tableNominal(vars = vars, weights = weights, subset = subset, group = group, 
    cap = "Table of nominal variables.", lab = "tab: nominal")

## do not include cumulative percentages
tableNominal(vars = vars, weights = weights, subset = subset, group = group, 
    cap = "Table of nominal variables.", lab = "tab: nominal", cumsum = FALSE)     

\dontrun{
## caption placement at the top and repeat column headings on top of each page
## in the longtable format. Have to manually add another backslash to hline and endhead
## below (they are removed when compiling the help file)!
tableNominal(vars = vars, cap = "Table of nominal variables.", cumsum = FALSE, 
caption.placement = "top", longtable = TRUE, add.to.row = list(pos = list(0), 
command = "\\hline \\endhead ")
}
}
\keyword{manip}
\keyword{character}


