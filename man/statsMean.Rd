\name{statsMean}
\alias{statsMean}
\title{Calculates numeric sum}
\usage{
  statsMean(surveyorCode, stats_method = "statsMean")
}
\arguments{
  \item{surveyorCode}{An object of class "surveyorCode".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}

  \item{stats_method}{The name of the function, for audit
  trail}
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
  \code{\link{statsMedian}}, \code{\link{statsNetScore}},
  \code{\link{statsRank}}, \code{\link{statsSum}},
  \code{\link{statsText}}
}
\keyword{stats}

