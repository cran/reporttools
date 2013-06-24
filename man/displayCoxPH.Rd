\name{displayCoxPH}
\alias{displayCoxPH}
\title{Function to display a coxph() object}
\description{Generate a LaTeX table of a \code{coxph} object. To be used in a Sweave document.}
\usage{displayCoxPH(mod, cap = "", lab = "mod", dig.coef = 2, dig.p = 1)}
\arguments{
  \item{mod}{\code{coxph} object.}
  \item{cap}{The function provides an automatic caption displaying the number of observations and events in \code{mod}. If \code{cap != ""} this string
  is added to the default caption.}
  \item{lab}{The LaTeX label for the generated table.}
  \item{dig.coef}{The number of significant digits for the estimated coefficients and the hazard ratios.}
  \item{dig.p}{The number of significant digits for \eqn{p}-values.}
}
\value{Returns a LaTeX table containing columns with the estimated coefficients, hazard ratios, 95 percent confidence intervals for the hazard ratios and 
the \eqn{p}-values.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}
\examples{
\dontrun{
# use example from coxph() in library 'survival'
test1 <- list(time = c(4, 3, 1, 1, 2, 2, 3),  
              status = c(1, 1, 1, 0, 1, 1, 0),  
              x = c(0, 2, 1, 1, 1, 0, 0),  
              sex = c(0, 0, 0, 0, 1, 1, 1)) 

# fit a coxph() model
library(survival)
mod1 <- coxph(Surv(time, status) ~ x + sex, data = test1) 

# generate table to insert in Sweave file
m1 <- displayCoxPH(mod1)
}
}

\keyword{manip}
\keyword{character}

