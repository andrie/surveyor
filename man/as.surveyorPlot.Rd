\name{as.surveyorPlot}
\alias{as.surveyorPlot}
\title{Creates surveyorPlot object and adds plot title.}
\usage{
  as.surveyorPlot(plot, surveyorStats, expansion = 1,
    plotFunction = "", plotSize = par("din"),
    addPlotTitle = surveyorStats$surveyorDefaults$addPlotTitle,
    ...)
}
\arguments{
  \item{plot}{A \code{ggplot} or \code{lattice} object}

  \item{surveyorStats}{An \code{\link{as.surveyorStats}}
  object}

  \item{expansion}{Multiplier for plot vertical dimension}

  \item{plotFunction}{String. The plot function that was
  used to create the plot. This is used purely to keep an
  audit trail of how the final output was created.}

  \item{plotSize}{Numeric vector of length 2, specifying
  the width and height of the plot (in inches)}

  \item{addPlotTitle}{If TRUE, adds question text as plot
  title, otherwise the plot has no title}

  \item{...}{Ignored}
}
\value{
  A surveyorPlot object. This is a list of: \describe{
  \item{plot}{A \code{ggplot} or \code{lattice} object}
  \item{expansion}{Expansion factor. Used to adjust the
  vertical scale of the plot when there are many
  categories} \item{plotFunction}{String indicating which
  plot function created the plot. Useful for debugging}
  \item{qType}{See also \code{\link{qType}}}
  \item{qid}{Question identifier, e.g. "Q4"}
  \item{data}{Data used in the plot} \item{nquestion}{}
  \item{formatter}{Formatting function for axis, e.g.
  \code{\link{formatRound}}} }
}
\description{
  Creates \code{surveyorPlot} object, a container for
  either ggplot or lattice graphic. The method
  \code{print.surveyorPlot} knows how to print the final
  plot object
}
\seealso{
  \code{\link{as.surveyorStats}}, \code{\link{plotGuess}}

  \code{\link{surveyPlot}} to plot a surveyor object

  Other surveyPlot arguments: \code{\link{codeGuess}},
  \code{\link{statsGuess}}
}

