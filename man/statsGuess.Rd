\name{statsGuess}
\alias{statsGuess}
\title{Inspects data and guesses what type of analysis to do.}
\usage{
  statsGuess(surveyorCode, ...)
}
\arguments{
  \item{surveyorCode}{An object of class "surveyorCode".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}

  \item{...}{Passed to relevant stats function}
}
\value{
  A data frame with three columns: cbreak, variable, value
}
\description{
  If data is categorical statsBin, if data is metric then
  statsSum
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other statsFunctions: \code{\link{statsBin}},
  \code{\link{statsBinPercent}},
  \code{\link{statsCentral}}, \code{\link{statsCount}},
  \code{\link{statsMean}}, \code{\link{statsMedian}},
  \code{\link{statsNetScore}}, \code{\link{statsRank}},
  \code{\link{statsSum}}, \code{\link{statsText}}
}
\keyword{stats}

