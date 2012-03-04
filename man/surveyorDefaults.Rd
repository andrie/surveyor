\name{surveyorDefaults}
\alias{surveyorDefaults}
\title{Initialises surveyor object defaults.}
\usage{
  surveyorDefaults(outputType = c("latex", "ppt", "device"),
    defaultThemeSize = 9,
    plotType = c("ggplot", "lattice"),
    fastgraphics = plotType[1] == "lattice",
    graphicFormat = c("pdf", "wmf"),
    addPlotTitle = outputType \%in\% c("ppt", "device"),
    defaultBrewerPal = "Set2", revBrewerPal = FALSE,
    printTable = TRUE)
}
\arguments{
  \item{outputType}{Character string specifying the
  destination of output: "latex", "ppt" or "device".  If
  "device", graphs are sent to the default device
  (typically the RGgui plot terminal)}

  \item{defaultThemeSize}{Text size in points, passed to
  ggplot}

  \item{plotType}{Either \code{ggplot} or \code{lattice}}

  \item{fastgraphics}{Uses lattice graphics if true,
  otherwise ggplot}

  \item{graphicFormat}{Device type for saving graphic
  plots.  Currently only pdf and wmf is supported.}

  \item{addPlotTitle}{If true, adds question text as plot
  title. Defaults to TRUE if \code{ouputType} is either
  "ppt" or "device"}

  \item{defaultBrewerPal}{Names of ColorBrewer palette to
  use}

  \item{revBrewerPal}{If TRUE, reverse the order of
  \code{defaultBrewerPal}}

  \item{printTable}{If TRUE will print the table as part of
  the report}
}
\description{
  Initialises surveyor object defaults.
}
\examples{
s <- surveyorDefaults()
s <- surveyorDefaults(outputType="latex")
}
\seealso{
  \code{\link{as.surveyor}},
  \code{\link{surveyorUpdateDefaults}}
}

