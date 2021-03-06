\name{splitPercentCombine}
\alias{splitPercentCombine}
\title{Splits a data.frame, applies a function and combines into data.frame.}
\usage{
  splitPercentCombine(dat, statsFunction = weightedCount)
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
  splitMeanCombine

  \code{\link{as.surveyorStats}}

  Other stats helper functions: \code{\link{allNA}},
  \code{\link{allNull}}, \code{\link{is.yesno}},
  \code{\link{qType}}, \code{\link{reorderQuestion}},
  \code{\link{reorderResponse}},
  \code{\link{splitBinCombine}},
  \code{\link{splitMeanCombine}},
  \code{\link{weightedCount}}, \code{\link{weightedMean}},
  \code{\link{weightedMedian}}, \code{\link{weightedSum}}
}
\keyword{Internal}

