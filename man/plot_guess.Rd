\name{plot_guess}
\alias{plot_guess}
\title{Guesses which plot format is optimal}
\usage{
  plot_guess(s, ...)
}
\arguments{
  \item{s}{A surveyor_stats object}

  \item{...}{Other parameters passed to specific plot
  function}
}
\description{
  Investigates columns in supplied data, and then chooses
  either \code{\link{plot_bar}} or
  \code{\link{plot_column}}
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other plot_functions: \code{\link{plot_bar}},
  \code{\link{plot_bar_sum}}, \code{\link{plot_column}},
  \code{\link{plot_net_score}}, \code{\link{plot_point}},
  \code{\link{plot_text}}
}
\keyword{plot}

