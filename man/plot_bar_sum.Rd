\name{plot_bar_sum}
\alias{plot_bar_sum}
\title{Plot data in bar chart format without modifying format.}
\usage{
  plot_bar_sum(s, plot_function = "plot_bar_sum", ...)
}
\arguments{
  \item{s}{A surveyor_stats object}

  \item{plot_function}{Character vector: Identifies the
  name of the plot function used to create the plot}

  \item{...}{Ignored}
}
\description{
  The standard plot_bar() function will plot the data in a
  stacked bar chart format and apply percentage formatting.
  plot_bar_sum() applies no formatting.
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other plot_functions: \code{\link{plot_bar}},
  \code{\link{plot_column}}, \code{\link{plot_guess}},
  \code{\link{plot_net_score}}, \code{\link{plot_point}},
  \code{\link{plot_text}}
}
\keyword{plot}

