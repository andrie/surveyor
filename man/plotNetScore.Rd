\name{plotNetScore}
\alias{plotNetScore}
\title{Plot data in net score format (bar chart, but percentage axis).}
\usage{
  plotNetScore(s, plotFunction = "plotNetScore",
    width = 50, ...)
}
\arguments{
  \item{s}{A surveyorStats object}

  \item{plotFunction}{Character vector: Identifies the name
  of the plot function used to create the plot}

  \item{width}{Question text gets line-wrapped at width}

  \item{...}{Ignored}
}
\description{
  Plot data in net score format (bar chart, but percentage
  axis).
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other plotFunctions: \code{\link{plotBar}},
  \code{\link{plotBarSum}}, \code{\link{plotColumn}},
  \code{\link{plotGuess}}, \code{\link{plotPoint}},
  \code{\link{plotText}}
}
\keyword{plot}

