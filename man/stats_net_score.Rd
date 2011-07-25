\name{stats_net_score}
\alias{stats_net_score}
\title{Code survey data as net score.}
\usage{
  stats_net_score(surveyor_code)
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
  Code survey data in net score form
}
\seealso{
  Stats functions: \itemize{ \item \code{\link{stats_bin}}
  \item \code{\link{stats_rank}} \item
  \code{\link{stats_net_score}} }

  For an overview of the surveyor package
  \code{\link{surveyor}}
}
\keyword{stats}

