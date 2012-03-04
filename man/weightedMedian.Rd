\name{weightedMedian}
\alias{weightedMedian}
\title{Calculates weighted median.}
\usage{
  weightedMedian(x, w, na.rm = TRUE,
    interpolate = is.null(ties), ties = NULL,
    method = c("quick", "shell"), ...)
}
\arguments{
  \item{x}{a numeric vector containing the values whose
  weighted median is to be computed.}

  \item{w}{a vector of weights the same length as x giving
  the weights to use for each element of x. Negative
  weights are treated as zero weights. Default value is
  equal weight to all values.#}

  \item{na.rm}{a logical value indicating whether NA values
  in x should be stripped before the computation proceeds,
  or not. If NA, no check at all for NAs is done. Default
  value is NA (for efficiency).}

  \item{interpolate}{If TRUE, linear interpolation is used
  to get a consistent estimate of the weighted median.}

  \item{ties}{If interpolate == FALSE, a character string
  specifying how to solve ties between two x's that are
  satisfying the weighted median criteria. Note that at
  most two values can satisfy the criteria. When ties is
  "min", the smaller value of the two is returned and when
  it is "max", the larger value is returned. If ties is
  "mean", the mean of the two values is returned and if it
  is "both", both values are returned. Finally, if ties is
  "weighted" (or NULL) a weighted average of the two are
  returned, where the weights are weights of all values
  x[i] <= x[k] and x[i] >= x[k], respectively.}

  \item{method}{If "shell", then order() is used and when
  method="quick", then internal qsort() is used.}

  \item{...}{Not used.}
}
\description{
  Computes a weighted median of a numeric vector.
}
\note{
  This function is a duplicate from
  \code{\link[aroma.light]{weightedMedian}} in the
  aroma.light package
}
\seealso{
  Other "central tendency functions":
  \code{\link{weightedCount}}, \code{\link{weightedMean}},
  \code{\link{weightedSum}}
}

