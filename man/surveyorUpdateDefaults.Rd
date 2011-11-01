\name{surveyorUpdateDefaults}
\alias{surveyorUpdateDefaults}
\title{Selectively updates surveyor defaults.}
\usage{
  surveyorUpdateDefaults(surveyor, outputToLatex = NULL,
  defaultThemeSize = NULL, plotType = NULL, fastgraphics =
  NULL, graphicFormat = NULL, addPlotTitle = NULL,
  defaultColourArea = NULL, defaultColourPoint = NULL,
  printTable = NULL)
}
\arguments{
  \item{outputToLatex}{TRUE or FALSE, determines if latex
  commands is output}

  \item{defaultThemeSize}{Text size in points, passed to
  ggplot}

  \item{plotType}{Either \code{ggplot} or \code{lattice}}

  \item{fastgraphics}{Uses lattice graphics if true,
  otherwise ggplot}

  \item{graphicFormat}{Device type for saving graphic
  plots.  Currently only pdf and wmf is supported.}

  \item{addPlotTitle}{If true, adds question text as plot
  title}

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

