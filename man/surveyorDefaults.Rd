\name{surveyorDefaults}
\alias{surveyorDefaults}
\title{Initialises surveyor object defaults.}
\usage{
  surveyorDefaults(defaultThemeSize = 9,
    plotType = c("ggplot", "lattice"),
    fastgraphics = plotType[1] == "lattice",
    addPlotTitle = TRUE, defaultBrewerPal = "Set2",
    revBrewerPal = FALSE, printTable = TRUE)
}
\arguments{
  \item{defaultThemeSize}{Text size in points, passed to
  ggplot}

  \item{plotType}{Either \code{ggplot} or \code{lattice}}

  \item{fastgraphics}{Uses lattice graphics if true,
  otherwise ggplot}

  \item{addPlotTitle}{If true, adds question text as plot
  title.}

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
s <- surveyorDefaults(defaultThemeSize=12)
}
\seealso{
  \code{\link{as.surveyor}},
  \code{\link{surveyorUpdateDefaults}}
}

