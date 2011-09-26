\name{statsNetScore}
\alias{statsNetScore}
\title{Code survey data as net score.}
\usage{
  statsNetScore(surveyorCode)
}
\arguments{
  \item{surveyorCode}{An object of class "surveyorCode".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}
}
\value{
  data frame
}
\description{
  Code survey data in net score form
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other statsFunctions: \code{\link{statsBin}},
  \code{\link{statsBinOld}}, \code{\link{statsBinPercent}},
  \code{\link{statsCentral}}, \code{\link{statsGuess}},
  \code{\link{statsMean}}, \code{\link{statsMedian}},
  \code{\link{statsRank}}, \code{\link{statsSum}},
  \code{\link{statsText}}
}
\keyword{stats}

