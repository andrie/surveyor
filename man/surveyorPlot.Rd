\name{surveyorPlot}
\alias{surveyorPlot}
\title{Codes and plots a survey question.}
\usage{
  surveyorPlot(surveyor, q_id,
    codeFunction = codeQuickArray,
    statsFunction = statsGuess, plotFunction = plotGuess,
    plotSize = if (outputType == "device") par("din") else surveyor$braid$defaultPlotSize,
    outputType = surveyor$defaults$outputType,
    onlyBreaks = seq_along(surveyor$crossbreak),
    plotMultiplierLimits = if (outputType == "ppt") c(0.8, 1.2) else c(0.8, 2.5),
    ...)
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

  \item{outputType}{"latex", "ppt" or "device": Specifies
  destination of ouput}

  \item{onlyBreaks}{Numeric vector that limits crossbreak
  processing}

  \item{plotMultiplierLimits}{Numeric vector of length two,
  indicating lower and upper limit of vertical plot
  resizing}

  \item{...}{Other parameters passed to codeFunction}
}
\description{
  This is the top level function that determines how a
  question is processed, coded, printed and plotted.
}
\seealso{
  \code{\link{as.surveyor}}
}

