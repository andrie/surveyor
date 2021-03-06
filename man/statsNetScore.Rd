\name{statsNetScore}
\alias{statsNetScore}
\title{Code survey data as net score.}
\usage{
  statsNetScore(surveyorCode, formatter = "formatPercent",
    ...)
}
\arguments{
  \item{surveyorCode}{An object of class "surveyorCode".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}

  \item{formatter}{A formatting function, such as
  \code{\link{formatPercent}}}

  \item{...}{other arguments not used by this method}
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
  \code{\link{statsBinPercent}},
  \code{\link{statsCentral}}, \code{\link{statsCount}},
  \code{\link{statsGuess}}, \code{\link{statsMean}},
  \code{\link{statsMedian}}, \code{\link{statsRank}},
  \code{\link{statsSum}}, \code{\link{statsText}}
}
\keyword{stats}

