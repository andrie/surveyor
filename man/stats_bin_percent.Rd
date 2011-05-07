\name{stats_bin_percent}
\alias{stats_bin_percent}
\title{Calculates summary statistics...}
\usage{stats_bin_percent(x)}
\description{Calculates summary statistics}
\details{Wrapper around \code{\link{stats_bin}}, binning statistics and calculating percentage

The results are sorted in descending order of value, and "response" is coerced into an ordered factor (unless "response" is already an ordered factor).}
\value{A data frame with three columns: cbreak, variable, value}
\seealso{Stats functions:
\itemize{
\item \code{\link{stats_bin}} 
\item \code{\link{stats_rank}} 
\item \code{\link{stats_net_score}}
}

For an overview of the surveyor package \code{\link{surveyor}}}
\keyword{stats}
\arguments{\item{x}{A data frame with four columns: cbreak, question, response, weight}}
