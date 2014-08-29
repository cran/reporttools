\name{tableContinuous}
\alias{tableContinuous}
\title{Generate a LaTeX table of descriptive statistics for continuous variables}
\description{
Many data analyses start with a display of descriptive statistics of important variables. This function takes
a data frame of continuous variables and possible grouping (such as e.g. treatment), weighting, and subset 
variables and provides a LaTeX table of descriptive statistics separately per group and jointly for all observations, 
per variable. User-defined statistics can be provided.
}
\usage{
tableContinuous(vars, weights = NA, subset = NA, group = NA, 
     stats = c("n", "min", "q1", "median", "mean", "q3", "max", 
     "s", "iqr", "na"), prec = 1, col.tit = NA,
     col.tit.font = c("bf", "", "sf", "it", "rm"), print.pval = 
     c("none", "anova", "kruskal"), pval.bound = 10^-4, 
     declare.zero = 10^-10, cap = "", lab = "", 
     font.size = "footnotesize", longtable = TRUE, 
     disp.cols = NA, nams = NA, ...)
}
\arguments{
  \item{vars}{A data frame containing continuous variables. See \code{nams} for an alternative
  way of specifying the variables to be displayed.}
  \item{weights}{Optional vector of weights of each observation.}
  \item{subset}{Optional logical vector, indicates subset of observations to be used.}
  \item{group}{Optional grouping variable.}
  \item{stats}{Specify which descriptive statistics should be displayed in the table, by either directly providing
  one or more of the default character strings (in arbitrary order) or a user-defined function. A user-defined
  function must bear a name, take a vector as an argument (\code{NA}'s are removed by default) and return
  a single number (the desired statistic). For details see the examples below.}
  \item{prec}{Specify number of decimals to be displayed.}
  \item{col.tit}{Specify titles of columns. Note that the length of this vector must be equal to the length of 
  \code{stats} plus the number of potential user-defined functions added
  to \code{stats}.}
  \item{col.tit.font}{If \code{col.tit} has not been specified, choose the
    font for the column titles here (default: no special font face).}
  \item{print.pval}{If \code{print.pval == "anova"}, \eqn{p}-values for an analysis of variance for a location
  difference between groups are added to the table. If \code{print.pval == "kruskal"}, \eqn{p}-values of a Kruskal-Wallis test
  are given. If \code{group} has only two levels, the respective \eqn{p}-values of a \eqn{t}- or Mann-Whitney test
  are provided. Only applies if \code{group} is provided. Note that by default, any missing values are removed for computation of \eqn{p}-values. If 
  missings should be considered a separate level, define the input variables accordingly.}
  \item{pval.bound}{\eqn{p}-values below \code{pval.bound} are formatted as \code{< pval.bound}.}
  \item{declare.zero}{Computed descriptive statistics (not \eqn{p}-values) below that constant are set to 0.
  Yields nicer tables, especially when displaying centered or standardized variables.}
  \item{cap}{The caption of the resulting LaTeX table.}
  \item{lab}{The label of the resulting LaTeX table.}
  \item{font.size}{Font size for the generated table in LaTeX.}
  \item{longtable}{If \code{TRUE}, function makes use of package \code{longtable} in LaTex to generate tables that 
  span more than one page. If \code{FALSE}, generates a table in \code{tabular} environment.}
  \item{disp.cols}{Only included for backward compatibility. Needs to be a vector built of (some of) the default statistics 
  character strings if not equal to \code{NA}. From package version 1.0.2 on use of \code{stats} is recommended.}
  \item{nams}{A vector of strings, containing the names corresponding to the variables in \code{vars}, if \code{vars}
  is not a data frame but a list of variables. These are then the names that appear in the LaTeX table.
  This option is only kept for backward compatibility.}
  \item{...}{Arguments pass through to \code{print.xtable}.}
}
\value{Outputs the LaTeX table.}
\note{If \code{longtable = TRUE} (which is the default), the function generates a table that may be more than one page 
long, you need to include the package longtable in the LaTeX source.

If a list of variables is given to \code{vars}, not all of these variables need to be of the same length. However, 
note the Warning below.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}
\references{
Rufibach, K. (2009) 
reporttools: R-Functions to Generate LaTeX Tables of Descriptive Statistics.
Journal of Statistical Software, Code Snippets, 31(1). \cr
\url{http://www.jstatsoft.org/v31/c01}.
}
\section{Warning}{If either one of the arguments \code{group}, \code{weights}, or \code{subset} is different from \code{NA}
and if \code{vars} is a list, then it is assumed that all variables 
in \code{vars} are of \emph{equal length}.}
\examples{
data(CO2)
vars <- CO2[, 4:5]
group <- CO2[, "Treatment"]
weights <- c(rep(1, 60), rep(0, 10), rep(2, 14))

## display default statistics, provide neither group nor weights
tableContinuous(vars = vars, stats = c("n", "min", "mean", "median", 
    "max", "iqr", "na"), print.pval = "kruskal", 
    cap = "Table of continuous variables.", lab = "tab: descr stat")

## display default statistics, only use a subset of observations, grouped analysis
tableContinuous(vars = vars, weights = weights, subset = 
    c(rep(TRUE, 57), rep(FALSE, 100 - 57)), group = group, prec = 3, print.pval = 
    "kruskal", cap = "Table of continuous variables.", lab = "tab: descr stat")

## supply user-defined statistics: trimmed mean and IQR as an unbiased estimate 
## of the population standard deviation in case of normal data
my.stats <- list("n", "na", "mean", "$\\\\bar{x}_{trim}$" = function(x){return(mean(x, 
    trim = .05))}, "iqr", "IQR.unbiased" = function(x){return(IQR(x) / 
    (2 * qnorm(3 / 4)))})
tableContinuous(vars = vars, weights = weights, group = group, stats = my.stats, 
    prec = 3, print.pval = "none", cap = "Table of continuous variables.", 
    lab = "tab: descr stat")

## disp.cols and nams can still be used, for backward compatibility.
## If a list is given to vars, the variables can be of different length. However,
## then weights, subset, and group must be set to NA (the default).
tableContinuous(vars = list(CO2$conc, CO2$uptake, rnorm(1111), runif(2222)), 
    nams = c("conc", "uptake", "random1", "random2"), disp.cols = 
    c("n", "min", "median", "max", "iqr", "na"), cap = "Table of continuous variables.", lab = 
    "tab: descr stat")
}
\keyword{manip}
\keyword{character}

