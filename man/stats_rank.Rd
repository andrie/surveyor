\name{stats_rank}
\alias{stats_rank}
\title{Calculates summary statistics for ranking type questions}
\usage{
  stats_rank(surveyor_code, top_n = 3)
}
\arguments{
  \item{surveyor_code}{An object of class "surveyor_code".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}

  \item{top_n}{Numeric, indicates how the ranking is
  summarised}
}
\value{
  A data frame with three columns: cbreak, variable, value
}
\description{
  Takes the result of a code_function, e.g. code_single(),
  and calculates summary values, for direct plotting by a
  plot_function, e.g. plot_bar()
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other stats_functions: \code{\link{stats_bin}},
  \code{\link{stats_bin_percent}},
  \code{\link{stats_guess}}, \code{\link{stats_mean}},
  \code{\link{stats_median}},
  \code{\link{stats_net_score}}, \code{\link{stats_sum}},
  \code{\link{stats_text}}
}
\keyword{stats}

