\name{stats_guess}
\alias{stats_guess}
\title{Inspects data and guesses what type of analysis to do.}
\usage{stats_guess(x)}
\description{Inspects data and guesses what type of analysis to do.}
\details{If data is categorical stats_bin, if data is metric then stats_sum}
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
