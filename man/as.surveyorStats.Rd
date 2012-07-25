\name{as.surveyorStats}
\alias{as.surveyorStats}
\title{Creates surveyorStats object, used as input to plot.}
\usage{
  as.surveyorStats(data, surveyorCode,
    ylabel = "Fraction of respondents",
    formatter = "formatPercent", nquestion = NULL,
    scale_breaks = NULL, stats_method = "",
    plotFunction = "", ...)
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

  \item{stats_method}{Character. Ddescription of calling
  statsFunction name - for audit trail}

  \item{plotFunction}{Character. Description of
  plotFunction to use}

  \item{...}{Ignored}
}
\value{
  A surveyorStats object. This is a list of: \describe{
  \item{data}{A data frame containing the summarized data}
  \item{surveyorDefaults}{A list with
  \code{\link{surveyorDefaults}}} \item{plotTitle}{The plot
  title to display} \item{qid}{Question id}
  \item{ylabel}{Plot y-axis label}
  \item{formatter}{Formatting function to use}
  \item{nquestion}{Number of subquestions. Used to
  determine vertical size of plot}
  \item{scale_breaks}{Scale breaks}
  \item{stats_method}{stats function - used for audit
  trail} \item{plotFunction}{plot function to use. See also
  \code{\link{plotGuess}}} \item{qtype}{Question type, as
  returned by \code{\link{qType}}} }
}
\description{
  A surveyorStats object contains all of the summarised
  information necessary to create a plot. This is the input
  data for \code{\link{as.surveyorPlot}}.
}
\seealso{
  \code{\link{as.surveyorPlot}}, \code{\link{statsGuess}}

  \code{\link{surveyPlot}} to plot a surveyor object
}

