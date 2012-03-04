\name{statsBinOld}
\alias{statsBinOld}
\title{Calculates summary statistics.}
\usage{
  statsBinOld(surveyorCode, ylabel = "Respondents",
    stats_method = "statsBin", convert_to_percent = FALSE)
}
\arguments{
  \item{surveyorCode}{An object of class "surveyorCode".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}

  \item{ylabel}{The label to print on y-axis of plots; used
  downstream}

  \item{stats_method}{A character vector describing name of
  stats method.  Used for audit trail}

  \item{convert_to_percent}{If true, will express results
  as fractions, rather than counts}
}
\value{
  A data frame with three columns: cbreak, variable, value
}
\description{
  Takes the result of a code_function, e.g. codeSingle(),
  and calculates summary values, for direct plotting by a
  plotFunction, e.g. plotBar()
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
  \code{\link{statsBinPercent}},
  \code{\link{statsCentral}}, \code{\link{statsCount}},
  \code{\link{statsGuess}}, \code{\link{statsMean}},
  \code{\link{statsMedian}}, \code{\link{statsNetScore}},
  \code{\link{statsRank}}, \code{\link{statsSum}},
  \code{\link{statsText}}
}
\keyword{stats}

