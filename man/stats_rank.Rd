\name{stats_rank}
\alias{stats_rank}
\title{Calculates summary statistics for ranking type questions...}
\usage{stats_rank(surveyor_code, top_n=3)}
\description{Calculates summary statistics for ranking type questions}
\details{Takes the result of a code_function, e.g. code_single(), and calculates
summary values, for direct plotting by a plot_function, e.g. plot_bar()}
\value{A data frame with three columns: cbreak, variable, value}
\seealso{Stats functions:
\itemize{
\item \code{\link{stats_bin}} 
\item \code{\link{stats_rank}} 
\item \code{\link{stats_net_score}}
}

For an overview of the surveyor package \code{\link{surveyor}}}
\keyword{stats}
\arguments{\item{surveyor_code}{An object of class "surveyor_code".  This is a list with the first element being a data frame with four columns: cbreak, question, response, weight}
\item{top_n}{Numeric, indicates how the ranking is summarised}}
