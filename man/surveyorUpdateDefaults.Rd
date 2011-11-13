\name{surveyorUpdateDefaults}
\alias{surveyorUpdateDefaults}
\title{Selectively updates surveyor defaults.}
\usage{
  surveyorUpdateDefaults(surveyor, outputType = NULL,
  defaultThemeSize = NULL, plotType = NULL, fastgraphics =
  NULL, graphicFormat = NULL, addPlotTitle = NULL,
  defaultColourArea = NULL, defaultColourPoint = NULL,
  printTable = NULL)
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

  \item{defaultColourArea}{Default RGB colour for areas in
  graphs (e.g. bars)}

  \item{defaultColourPoint}{Default RGB colour for points
  in graphs (e.g. points)}

  \item{printTable}{If TRUE will print the table as part of
  the report}
}
\description{
  Selectively updates surveyor defaults.
}
\seealso{
  \code{\link{as.surveyor}}, \code{\link{surveyorDefaults}}
}

