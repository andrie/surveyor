\name{statsMedian}
\alias{statsMedian}
\title{Calculates numeric median.}
\usage{
  statsMedian(surveyorCode, ylabel = "Median", ...)
}
\arguments{
  \item{surveyorCode}{An object of class "surveyorCode".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}

  \item{ylabel}{y-axis label on plot}

  \item{...}{Other arguments passed to
  \code{\link{as.surveyorStats}}}
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
  \code{\link{statsBinOld}}, \code{\link{statsBinPercent}},
  \code{\link{statsCentral}}, \code{\link{statsGuess}},
  \code{\link{statsMean}}, \code{\link{statsNetScore}},
  \code{\link{statsRank}}, \code{\link{statsSum}},
  \code{\link{statsText}}
}
\keyword{stats}

