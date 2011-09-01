\name{stats_median}
\alias{stats_median}
\title{Calculates median}
\usage{
  stats_median(surveyor_code, fun = "weighted_median",
  stats_method = "stats_median", yLabel = "Median value")
}
\arguments{
  \item{surveyor_code}{An object of class "surveyor_code".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}

  \item{fun}{The function to use to calculate central
  tendency, e.g. mean, median or sum}

  \item{stats_method}{The name of the function, for audit
  trail}

  \item{yLabel}{y axis label}
}
\value{
  A data frame with three columns: cbreak, variable, value
}
\description{
  Add description
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other stats_functions: \code{\link{stats_bin}},
  \code{\link{stats_bin_percent}},
  \code{\link{stats_guess}}, \code{\link{stats_mean}},
  \code{\link{stats_net_score}}, \code{\link{stats_rank}},
  \code{\link{stats_sum}}, \code{\link{stats_text}}
}
\keyword{stats}

