\name{stats_text}
\alias{stats_text}
\title{Code survey data as text.}
\usage{
  stats_text(surveyor_code)
}
\arguments{
  \item{surveyor_code}{An object of class "surveyor_code".
  This is a list with the first element being a data frame
  with four columns: cbreak, question, response, weight}
}
\value{
  data frame
}
\description{
  Code survey data in text form
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other stats_functions: \code{\link{stats_bin}},
  \code{\link{stats_bin_percent}},
  \code{\link{stats_guess}}, \code{\link{stats_mean}},
  \code{\link{stats_median}},
  \code{\link{stats_net_score}}, \code{\link{stats_rank}},
  \code{\link{stats_sum}}
}
\keyword{stats}

