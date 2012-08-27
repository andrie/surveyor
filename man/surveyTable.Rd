\name{surveyTable}
\alias{surveyTable}
\title{Wrapper around surveyPlot that suppresses plot creation and returns data only.}
\usage{
  surveyTable(x, qid, statsFunction = "statsGuess",
    plotFunction = NA, ...)
}
\arguments{
  \item{x}{Object to plot, of class \code{surveyor}, see
  also \code{\link{as.surveyor}}}

  \item{qid}{String. Unique question identifier, e.g. "Q4"}

  \item{statsFunction}{A reference to a function that
  summarizes the coded data. Defaults to
  \code{\link{statsGuess}}}

  \item{plotFunction}{A reference to a function that plots
  the summarized data. Defaults to \code{\link{plotGuess}}}

  \item{...}{Passed to methods}
}
\description{
  Wrapper around surveyPlot that suppresses plot creation
  and returns data only.
}

