\name{stats_guess}
\alias{stats_guess}
\title{Inspects data and guesses what type of analysis to do.}
\usage{
  stats_guess(surveyor_code)
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
  If data is categorical stats_bin, if data is metric then
  stats_sum
}
\seealso{
  Stats functions: \itemize{ \item \code{\link{stats_bin}}
  \item \code{\link{stats_rank}} \item
  \code{\link{stats_net_score}} }

  For an overview of the surveyor package
  \code{\link{surveyor}}
}
\keyword{stats}

