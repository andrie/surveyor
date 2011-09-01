\name{as.surveyor_stats}
\alias{as.surveyor_stats}
\title{Creates surveyor_stats object, used as input to plot.}
\usage{
  as.surveyor_stats( data, surveyor_code, ylabel="Fraction
  of respondents", formatter="percent", nquestion,
  scale_breaks, stats_method="", plot_function="")
}
\arguments{
  \item{data}{A data frame}

  \item{surveyor_code}{A surveyor_code object}

  \item{ylabel}{Character string to print as plot y label}

  \item{formatter}{Name of a formatting function}

  \item{nquestion}{Number of identifiable questions /
  responses, used for plot sizing downstream}

  \item{scale_breaks}{Vector that specifies breaks in
  ggplot}

  \item{stats_method}{Character description of calling
  function name - for audit trail}
}
\value{
  A surveyor_stats object
}
\description{
  Creates surveyor_stats object, used as input to plot
}
\keyword{internal}

