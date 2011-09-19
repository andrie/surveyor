\name{statsMedian}
\alias{statsMedian}
\title{Calculates median}
\usage{
  statsMedian(surveyorCode, fun = "weightedMedian",
  stats_method = "statsMedian", yLabel = "Median value")
}
\arguments{
  \item{surveyorCode}{An object of class "surveyorCode".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}

  \item{fun}{The function to use to calculate central
  tendency, e.g. mean, median or sum}

  \item{stats_method}{The name of the function, for audit
  trail}

  \item{yLabel}{y axis label}
}
\value{
  A data frame with three columns: cbreak, variable, value
}
\description{
  Add description
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other statsFunctions: \code{\link{statsBin}},
  \code{\link{statsBinPercent}}, \code{\link{statsGuess}},
  \code{\link{statsMean}}, \code{\link{statsNetScore}},
  \code{\link{statsRank}}, \code{\link{statsSum}},
  \code{\link{statsText}}
}
\keyword{stats}

