\name{splitMeanCombine}
\alias{splitMeanCombine}
\title{Splits a data.frame, applies a function and combines into data.frame.}
\usage{
  splitMeanCombine(dat, statsFunction = weightedMean)
}
\arguments{
  \item{dat}{a \code{\link{as.surveyorCode}} object}

  \item{statsFunction}{A function that calculates a weighed
  statistic, such as \code{\link{weightedMean}},
  \code{\link{weightedMedian}}, \code{\link{weightedSum}}
  or \code{\link{weightedCount}}}
}
\description{
  This is a fast implementation of
  \code{\link[plyr]{ddply}}, optimised for a
  \code{\link{as.surveyorCode}} object.
}
\seealso{
  splitBinCombine
}
\keyword{Internal}

