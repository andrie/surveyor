\name{surveyPlot}
\alias{surveyPlot}
\alias{surveyPlot.surveyor}
\title{Codes and plots a survey question.}
\usage{
  surveyPlot(x, qid, statsFunction = "statsGuess",
    plotFunction = "plotGuess",
    codeFunction = "codeQuickArray", crossbreak,
    onlyBreaks = seq_along(x$crossbreak), ...)

  \method{surveyPlot}{surveyor} (x, qid,
    statsFunction = "statsGuess",
    plotFunction = "plotGuess",
    codeFunction = "codeQuickArray",
    crossbreak = x$crossbreak,
    onlyBreaks = seq_along(x$crossbreak), ...)
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

  \item{codeFunction}{A reference to a function that
  processes the question data. Defaults to
  \code{\link{codeGuess}}}

  \item{crossbreak}{A list of crossbreak vectors (each must
  be a named factor).  See also \code{\link{as.surveyor}}}

  \item{onlyBreaks}{Numeric vector that limits crossbreak
  processing}

  \item{...}{Passed to methods}
}
\value{
  Depends on method, but typically a list of
  \code{\link{as.surveyorPlot}} objects
}
\description{
  This function takes extracts the data from a
  \code{surveydata} object, then analyses and plots it.
}
\details{
  This is a generic function, with an implementation for
  class \code{surveyor}, see also
  \code{\link{as.surveyor}}. The package \code{surveybraid}
  implements additional methods for \code{surveybraid} and
  \code{surveybraidppt}.  See also
  \code{\link[surveybraid]{as.surveybraid}} and
  \code{\link[surveybraid]{as.surveybraidppt}}.
}
\seealso{
  \code{\link{as.surveyor}}
}

