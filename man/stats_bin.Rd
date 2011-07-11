\name{stats_bin}
\alias{stats_bin}
\title{Calculates summary statistics.}
\usage{stats_bin(surveyor_code, ylabel="Respondents",
    stats_method="stats_bin", convert_to_percent=FALSE)}
\description{Calculates summary statistics.}
\details{Takes the result of a code_function, e.g. code_single(), and calculates summary values, for direct plotting by a plot_function, e.g. plot_bar()

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
\arguments{\item{surveyor_code}{An object of class "surveyor_code".  This is a list with the first element being a data frame with four columns: cbreak, question, response, weight}
\item{ylabel}{The label to print on y-axis of plots; used downstream}
\item{stats_method}{A character vector describing name of stats method.  Used for audit trail}
\item{convert_to_percent}{If true, will express results as fractions, rather than counts}}
