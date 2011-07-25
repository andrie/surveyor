\name{stats_mean}
\alias{stats_mean}
\title{Calculates numeric sum}
\usage{
  stats_mean(surveyor_code, fun = "mean", stats_method =
  "stats_mean")
}
\arguments{
  \item{surveyor_code}{An object of class "surveyor_code".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}

  \item{fun}{The function to use to calculate central
  tendency, e.g. mean, median or sum}

  \item{stats_function}{The name of the function, for audit
  trail}
}
\value{
  A data frame with three columns: cbreak, variable, value
}
\description{
  Add description
}
\seealso{
  Stats functions: \itemize{ \item
  \code{\link{stats_median}} \item \code{\link{stats_sum}}
  \item \code{\link{stats_bin}} \item
  \code{\link{stats_rank}} \item
  \code{\link{stats_net_score}} }

  For an overview of the surveyor package
  \code{\link{surveyor}}
}
\keyword{stats}

