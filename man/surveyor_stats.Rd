\name{surveyor_stats}
\alias{surveyor_stats}
\title{Creates surveyor_stats object, used as input to plot.}
\usage{surveyor_stats(data, ylabel="Fraction of respondents",
    formatter="percent", nquestion=length(unique(data$question)),
    scale_breaks, stats_method="")}
\description{Creates surveyor_stats object, used as input to plot.}
\details{Creates surveyor_stats object, used as input to plot}
\value{A surveyor_stats object}
\keyword{internal}
\arguments{\item{data}{A data frame}
\item{ylabel}{Character string to print as plot y label}
\item{formatter}{Name of a formatting function}
\item{nquestion}{Number of identifiable questions, used for plot sizing downstream}
\item{scale_breaks}{Vector that specifies breaks in ggplot}
\item{stats_method}{Character description of calling function name - for audit trail}}
