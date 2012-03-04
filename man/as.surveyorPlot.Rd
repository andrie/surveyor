\name{as.surveyorPlot}
\alias{as.surveyorPlot}
\title{Creates surveyorPlot object and adds plot title.}
\usage{
  as.surveyorPlot(plot, surveyorStats, expansion = 1,
    plotFunction = "", plotSize = par("din"), ...)
}
\arguments{
  \item{plot}{A ggplot or lattice object}

  \item{expansion}{Multiplier for plot vertical dimension}

  \item{plotFunction}{The plot function that was used to
  create the plot}
}
\value{
  A surveyorPlot object
}
\description{
  Creates surveyorPlot object, a container for either
  ggplot or lattice graphic.
}
\keyword{internal}

