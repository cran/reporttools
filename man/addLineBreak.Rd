\name{addLineBreak}
\alias{addLineBreak}
\title{Break lines in a text column of a dataframe.}
\description{Given a dataframe with a column containing character string, generate a new dataframe where these strings have a maximal length. Useful when embedding dataframes in a Sweave document, without having it overlapping page width.}
\usage{addLineBreak(tab, length, col)}
\arguments{
\item{tab}{Dataframe containing the data.}
\item{length}{Maximal length to which strings should be broken.}
\item{col}{Column of \code{tab} that contains strings.}
}
\value{List with two elements: The resulting dataframe with lines broken, and a vector that gives row where each entry in the new dataframe ends. The latter is useful when horizontal lines should be added when using \link{xtable}.}
\author{Kaspar Rufibach (maintainer), \email{kaspar.rufibach@gmail.com}, \cr \url{http://www.kasparrufibach.ch}}
\examples{
tab <- data.frame(cbind(1:4))
tab[1, 2] <- paste(letters, sep = "", collapse = "")
tab[3, 2] <- paste(LETTERS, sep = "", collapse = "")
tab[c(2, 4), 2] <- ""
colnames(tab) <- c("nr", "text")

addLineBreak(tab, length = 12, col = 2)
}

\keyword{manip}
\keyword{character}

