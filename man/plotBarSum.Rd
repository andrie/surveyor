\name{plotBarSum}
\alias{plotBarSum}
\title{Plot data in bar chart format without modifying format.}
\usage{
  plotBarSum(s, plotFunction = "plotBarSum", ...)
}
\arguments{
  \item{s}{A surveyorStats object}

  \item{plotFunction}{Character vector: Identifies the name
  of the plot function used to create the plot}

  \item{...}{Ignored}
}
\description{
  The standard plotBar() function will plot the data in a
  stacked bar chart format and apply percentage formatting.
  plotBarSum() applies no formatting.
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other plotFunctions: \code{\link{plotBar}},
  \code{\link{plotColumn}}, \code{\link{plotGuess}},
  \code{\link{plotNetScore}}, \code{\link{plotPoint}},
  \code{\link{plotText}}
}
\keyword{plot}

