\name{surveyorDefaults}
\alias{surveyorDefaults}
\title{Initialises surveyor object defaults.}
\usage{
  surveyorDefaults(outputType = c("latex", "ppt",
  "device"), defaultThemeSize = 9, plotType = c("ggplot",
  "lattice"), fastgraphics = plotType[1] == "lattice",
  graphicFormat = c("pdf", "wmf"), addPlotTitle = FALSE,
  defaultColourArea = rgb(127, 201, 127, 255, maxColorValue
  = 255), defaultColourPoint = rgb(27, 158, 119, 255,
  maxColorValue = 255), printTable = TRUE)
}
\arguments{
  \item{outputType}{TRUE or FALSE, determines if latex
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

