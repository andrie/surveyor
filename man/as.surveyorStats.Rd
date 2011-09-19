\name{as.surveyorStats}
\alias{as.surveyorStats}
\title{Creates surveyorStats object, used as input to plot.}
\usage{
  as.surveyorStats(data, surveyorCode, ylabel = 
  "Fraction of respondents", formatter = "percent", 
  nquestion = NULL, scale_breaks = NULL, 
  stats_method = "", plotFunction =  "")
}
\arguments{
  \item{data}{A data frame}

  \item{surveyorCode}{A surveyorCode object}

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
  A surveyorStats object
}
\description{
  Creates surveyorStats object, used as input to plot
}
\keyword{internal}

