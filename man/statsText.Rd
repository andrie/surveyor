\name{statsText}
\alias{statsText}
\title{Code survey data as text.}
\usage{
  statsText(surveyorCode, formatter = "format", ...)
}
\arguments{
  \item{surveyorCode}{An object of class "surveyorCode".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}

  \item{formatter}{A formatting function, such as
  \code{\link{formatPercent}}}

  \item{...}{Other arguments passed to
  \code{\link{as.surveyorStats}}}
}
\value{
  data frame
}
\description{
  Code survey data in text form
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other statsFunctions: \code{\link{statsBin}},
  \code{\link{statsBinPercent}},
  \code{\link{statsCentral}}, \code{\link{statsCount}},
  \code{\link{statsGuess}}, \code{\link{statsMean}},
  \code{\link{statsMedian}}, \code{\link{statsNetScore}},
  \code{\link{statsRank}}, \code{\link{statsSum}}
}
\keyword{stats}

