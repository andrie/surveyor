\name{as.surveyPlot}
\alias{as.surveyPlot}
\title{Creates surveyPlot object and adds plot title.}
\usage{
  as.surveyPlot(plot, surveyorStats, expansion = 1,
    plotFunction = "", plotSize = par("din"),
    addPlotTitle = surveyorStats$surveyorDefaults$addPlotTitle,
    ...)
}
\arguments{
  \item{plot}{A ggplot or lattice object}

  \item{expansion}{Multiplier for plot vertical dimension}

  \item{plotFunction}{The plot function that was used to
  create the plot}
}
\value{
  A surveyPlot object
}
\description{
  Creates surveyPlot object, a container for either ggplot
  or lattice graphic.
}
\keyword{internal}

