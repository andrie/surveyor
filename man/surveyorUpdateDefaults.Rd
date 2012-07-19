\name{surveyorUpdateDefaults}
\alias{surveyorUpdateDefaults}
\title{Selectively updates surveyor defaults.}
\usage{
  surveyorUpdateDefaults(surveyor, defaultThemeSize = NULL,
    fastgraphics = NULL, addPlotTitle = NULL,
    brewerPalette = NULL, revBrewerPal = NULL,
    printTable = NULL)
}
\arguments{
  \item{surveyor}{Surveyor object}

  \item{brewerPalette}{Names of ColorBrewer palette to use}

  \item{defaultThemeSize}{Text size in points, passed to
  ggplot}

  \item{fastgraphics}{Uses lattice graphics if true,
  otherwise ggplot}

  \item{addPlotTitle}{If true, adds question text as plot
  title.}

  \item{revBrewerPal}{If TRUE, reverse the order of
  \code{defaultBrewerPal}}

  \item{printTable}{If TRUE will print the table as part of
  the report}
}
\description{
  Selectively updates surveyor defaults.
}
\seealso{
  \code{\link{as.surveyor}}, \code{\link{surveyorDefaults}}
}

