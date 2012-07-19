\name{reorderQuestion}
\alias{reorderQuestion}
\title{Sorts data.frame questions in descending order by value.}
\usage{
  reorderQuestion(df, reverse = FALSE)
}
\arguments{
  \item{df}{A data frame containing at least two columns:
  response and value}
}
\value{
  A data frame
}
\description{
  Sorts data.frame question in descending order by value
}
\seealso{
  \code{\link{as.surveyorStats}}

  Other stats helper functions: \code{\link{allNA}},
  \code{\link{allNull}}, \code{\link{is.yesno}},
  \code{\link{qType}}, \code{\link{reorderResponse}},
  \code{\link{splitBinCombine}},
  \code{\link{splitMeanCombine}},
  \code{\link{splitPercentCombine}},
  \code{\link{weightedCount}}, \code{\link{weightedMean}},
  \code{\link{weightedMedian}}, \code{\link{weightedSum}}
}
\keyword{internal}

