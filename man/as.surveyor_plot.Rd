\name{as.surveyor_plot}
\alias{as.surveyor_plot}
\title{Creates surveyor_plot object.}
\usage{
  as.surveyor_plot(plot, surveyor_stats, expansion = 1,
  plot_function = "")
}
\arguments{
  \item{plot}{A ggplot or lattice object}

  \item{expansion}{Multiplier for plot vertical dimension}

  \item{plot_function}{The plot function that was used to
  create the plot}
}
\value{
  A surveyor_plot object
}
\description{
  Creates surveyor_plot object, a container for either
  ggplot or lattice graphic.
}
\keyword{internal}

