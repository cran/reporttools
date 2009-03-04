\name{tableContinuous}
\alias{tableContinuous}
\title{Generate a LaTeX table of descriptive statistics for continuous variables}
\description{
Many data analyses start with a display of descriptive statistics of important variables. This function takes
a list of numeric variables and a possible grouping variable (such as e.g. treatment) and provides
a LaTeX table of descriptive statistics separately per group and jointly for all observations, per variable. 
User-defined statistics can be provided.
}
\usage{
tableContinuous(vars, nams, group = NA, subset = NA, 
     stats = c("n", "min", "q1", "median", "mean", "q3", "max", 
     "s", "iqr", "na"), prec = 1, col.tit = NA, print.pval = 
     c("none", "anova", "kruskal")[1], declare.zero = 10^-10, 
     cap = "", lab = "", disp.cols = NA)
}
\arguments{
  \item{vars}{A list of continuous variables.}
  \item{nams}{A vector of strings, containing the names corresponding to the variables in \code{vars}. These are the names
  that appear in the LaTeX table.}
  \item{group}{Grouping variable, may be omitted.}
  \item{subset}{Only consider a subset of observations.}
  \item{stats}{Specify which descriptive statistics should be displayed in the table, by either directly providing
  one or more of the default character strings (in arbitrary order) or a user-defined function. A user-defined
  function must bear a name, take a vector as an argument (\code{NA}'s are removed by default) and return
  a single number (the desired statistic). For details see the examples below.}
  \item{prec}{Specify number of decimals to be displayed.}
  \item{col.tit}{Specify titles of columns.}
  \item{print.pval}{If \code{print.pval == "anova"}, a \eqn{p}-values for an analysis of variance for a location
  difference between groups is added to the table. If \code{print.pval == "kruskal"} a \eqn{p}-value of a Kruskal-Wallis test
  is given. If \code{group} has only two levels, the respective \eqn{p}-value of a \eqn{t}- or Mann-Whitney test
  is provided. Only applies if \code{group} is provided.}
  \item{declare.zero}{Computed descriptive statistics (not \eqn{p}-values) below that constant are set to 0.
  Yields nicer tables, especially when displaying centered or standardized variables.}
  \item{cap}{The caption of the resulting LaTeX table.}
  \item{lab}{The label of the resulting LaTeX table.}
  \item{disp.cols}{Only included for backward compatibility. Needs to be a vector built of (some of) the default statistics 
  character strings if not equal to \code{NA}. From package version 1.0.2 use of \code{stats} is recommended.}
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
vars <- list(c(rnorm(90), NA), rgamma(100, 2, 1))
nams <- c("Var1", "Var2")
group <- sample(c(rep("A", 50), rep("B", 50)))
subset <- c(1:70, 91:100)

## display default statistics
tableContinuous(vars, nams, group, subset, stats = c("n", "min", "q1", "mean", 
    "median", "q3", "max", "iqr", "na"), prec = 1, col.tit = NA, print.pval = 
    "kruskal", cap = "Table of continuous variables.", lab = "tab: descr stat")

## supply user-defined statistics: trimmed mean and IQR as an unbiased estimate 
## of the population standard deviation
my.stats <- list("n", "mean", "$\\bar{x}_{trim}$" = function(x){return(mean(x, 
    trim = .05))}, "iqr", "IQR.unbiased" = function(x){return(IQR(x) / 
    (2 * qnorm(3 / 4)))})
tableContinuous(vars, nams, group, subset, stats = my.stats, prec = 1, 
    col.tit = NA, print.pval = "kruskal", cap = "Table of continuous variables.", 
    lab = "tab: descr stat")

## disp.cols can still be used, for backward compatibility
tableContinuous(vars, nams, group, subset, disp.cols = c("n", "min", "median", 
    "max", "iqr", "na"), prec = 1, col.tit = NA, print.pval = "kruskal", 
    cap = "Table of continuous variables.", lab = "tab: descr stat")
}
\keyword{manip}
\keyword{character}

