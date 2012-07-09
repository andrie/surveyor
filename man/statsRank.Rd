\name{statsRank}
\alias{statsRank}
\title{Calculates summary statistics for ranking type questions}
\usage{
  statsRank(surveyorCode, top_n = 3, ...)
}
\arguments{
  \item{surveyorCode}{An object of class "surveyorCode".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}

  \item{top_n}{Numeric, indicates how the ranking is
  summarised}

  \item{...}{other arguments not used by this method}
}
\value{
  A data frame with three columns: cbreak, variable, value
}
\description{
  Takes the result of a code_function, e.g. codeSingle(),
  and calculates summary values, for direct plotting by a
  plotFunction, e.g. plotBar()
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other statsFunctions: \code{\link{statsBin}},
  \code{\link{statsBinPercent}},
  \code{\link{statsCentral}}, \code{\link{statsCount}},
  \code{\link{statsGuess}}, \code{\link{statsMean}},
  \code{\link{statsMedian}}, \code{\link{statsNetScore}},
  \code{\link{statsSum}}, \code{\link{statsText}}
}
\keyword{stats}

