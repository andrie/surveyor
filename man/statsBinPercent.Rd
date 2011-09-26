\name{statsBinPercent}
\alias{statsBinPercent}
\title{Calculates summary statistics}
\usage{
  statsBinPercent(surveyorCode, ...)
}
\arguments{
  \item{surveyorCode}{An object of class "surveyorCode".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}

  \item{...}{Passed to \code{\link{as.surveyorStats}}}
}
\value{
  A data frame with three columns: cbreak, variable, value
}
\description{
  Wrapper around \code{\link{statsBin}}, binning statistics
  and calculating percentage
}
\details{
  The results are sorted in descending order of value, and
  "response" is coerced into an ordered factor (unless
  "response" is already an ordered factor).
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other statsFunctions: \code{\link{statsBin}},
  \code{\link{statsBinOld}}, \code{\link{statsCentral}},
  \code{\link{statsGuess}}, \code{\link{statsMean}},
  \code{\link{statsMedian}}, \code{\link{statsNetScore}},
  \code{\link{statsRank}}, \code{\link{statsSum}},
  \code{\link{statsText}}
}
\keyword{stats}

