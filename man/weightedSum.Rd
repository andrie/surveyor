\name{weightedSum}
\alias{weightedSum}
\title{Calculates weighted sum.}
\usage{
  weightedSum(x, w, na.rm = TRUE)
}
\arguments{
  \item{x}{a numeric vector containing the values whose
  weighted sum is to be computed.}

  \item{w}{a vector of weights the same length as x giving
  the weights to use for each element of x. Negative
  weights are treated as zero weights. Default value is
  equal weight to all values.#}

  \item{na.rm}{a logical value indicating whether NA values
  in x should be stripped before the computation proceeds,
  or not. If NA, no check at all for NAs is done. Default
  value is NA (for efficiency).}
}
\description{
  Computes a weighted sum of a numeric vector.
}
\seealso{
  \code{\link{as.surveyorStats}}

  Other "central tendency functions":
  \code{\link{weightedCount}}, \code{\link{weightedMean}},
  \code{\link{weightedMedian}}

  Other stats helper functions: \code{\link{allNA}},
  \code{\link{allNull}}, \code{\link{is.yesno}},
  \code{\link{qType}}, \code{\link{reorderQuestion}},
  \code{\link{reorderResponse}},
  \code{\link{splitBinCombine}},
  \code{\link{splitMeanCombine}},
  \code{\link{splitPercentCombine}},
  \code{\link{weightedCount}}, \code{\link{weightedMean}},
  \code{\link{weightedMedian}}
}

