\name{statsCentral}
\alias{statsCentral}
\title{Calculates central tendency.}
\usage{
  statsCentral(surveyorCode, statsFunction =
  c("weightedMean", "median", "sum", "count"), ylabel =
  "Mean", ...)
}
\arguments{
  \item{surveyorCode}{An object of class "surveyorCode".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}

  \item{statsFunction}{The name of a weighted central
  tendency function, e.g. \code{\link{weightedMean}},
  \code{\link{weightedSum}}, \code{\link{weightedMedian}}
  or \code{\link{weightedCount}}}

  \item{ylabel}{y-axis label on plot}

  \item{...}{Other arguments passed to
  \code{\link{as.surveyorStats}}}
}
\value{
  A data frame with three columns: cbreak, question, value
}
\description{
  Add description
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other statsFunctions: \code{\link{statsBin}},
  \code{\link{statsBinOld}}, \code{\link{statsBinPercent}},
  \code{\link{statsGuess}}, \code{\link{statsMean}},
  \code{\link{statsMedian}}, \code{\link{statsNetScore}},
  \code{\link{statsRank}}, \code{\link{statsSum}},
  \code{\link{statsText}}
}
\keyword{stats}

