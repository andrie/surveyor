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
  Plot functions: \itemize{ \item \code{\link{plot_bar}}
  \item \code{\link{plot_bar_sum}} \item
  \code{\link{plot_column}} \item \code{\link{plot_point}}
  \item \code{\link{plot_text}} \item
  \code{\link{plot_net_score}} }

  For an overview of the surveyor package
  \code{\link{surveyor}}
}
\keyword{plot}

