\name{surveyPlot}
\alias{surveyPlot}
\alias{surveyPlot.surveyor}
\title{Codes and plots a survey question.}
\usage{
  surveyPlot(x, qid, statsFunction = "statsGuess",
    plotFunction = "plotGuess",
    codeFunction = "codeQuickArray",
    onlyBreaks = seq_along(x$crossbreak), ...)

  \method{surveyPlot}{surveyor} (x, qid,
    statsFunction = "statsGuess",
    plotFunction = "plotGuess",
    codeFunction = "codeQuickArray",
    onlyBreaks = seq_along(x$crossbreak), ...)
}
\arguments{
  \item{x}{Object}

  \item{qid}{Question id}

  \item{codeFunction}{A reference to a function that
  processes the question data}

  \item{statsFunction}{A reference to a function that
  summarizes the coded data}

  \item{plotFunction}{A reference to a function that plots
  the summarized data}

  \item{onlyBreaks}{Numeric vector that limits crossbreak
  processing}

  \item{...}{Passed to methods}
}
\value{
  Depends on method
}
\description{
  Codes and plots a survey question.
}
\seealso{
  \code{\link{as.surveyor}}
}

