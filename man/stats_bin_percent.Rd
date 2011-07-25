\name{stats_bin_percent}
\alias{stats_bin_percent}
\title{Calculates summary statistics}
\usage{
  stats_bin_percent(surveyor_code)
}
\arguments{
  \item{surveyor_code}{An object of class "surveyor_code".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}
}
\value{
  A data frame with three columns: cbreak, variable, value
}
\description{
  Wrapper around \code{\link{stats_bin}}, binning
  statistics and calculating percentage
}
\details{
  The results are sorted in descending order of value, and
  "response" is coerced into an ordered factor (unless
  "response" is already an ordered factor).
}
\seealso{
  Stats functions: \itemize{ \item \code{\link{stats_bin}}
  \item \code{\link{stats_rank}} \item
  \code{\link{stats_net_score}} }

  For an overview of the surveyor package
  \code{\link{surveyor}}
}
\keyword{stats}

