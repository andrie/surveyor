\name{surveyorPlot}
\alias{surveyorPlot}
\title{Codes and plots a survey question.}
\usage{
  surveyorPlot(surveyor, q_id, codeFunction =
  codeQuickArray, statsFunction = statsGuess, plotFunction
  = plotGuess, plotSize = surveyor$braid$defaultPlotSize,
  outputToLatex = surveyor$defaults$outputToLatex,
  onlyBreaks = seq_along(surveyor$crossbreak), ...)
}
\arguments{
  \item{surveyor}{Surveyor object}

  \item{q_id}{Question id}

  \item{codeFunction}{A reference to a function that
  processes the question data}

  \item{statsFunction}{A reference to a function that
  summarizes the coded data}

  \item{plotFunction}{A reference to a function that plots
  the summarized data}

  \item{plotSize}{Size in inches of plot output, e.g.
  c(4,3)}

  \item{outputToLatex}{If TRUE, wraps output in latex code,
  otherwise print to console}

  \item{onlyBreaks}{Numeric vector that limits crossbreak
  processing}

  \item{...}{Other parameters passed to codeFunction}
}
\description{
  This is the top level function that determines how a
  question is processed, coded, printed and plotted.
}
\seealso{
  \code{\link{as.surveyor}}
}

