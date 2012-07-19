\name{as.surveyorCode}
\alias{as.surveyorCode}
\title{Creates object of class "surveyorCode".}
\usage{
  as.surveyorCode(x, surveyor, qid, ...)
}
\arguments{
  \item{x}{Data created by surveyor code function}

  \item{surveyor}{A surveyor object}

  \item{qid}{Question identifier, e.g. "Q4"}

  \item{...}{Passed to surveyorStats}
}
\value{
  An object of class "surveyorCode" \describe{
  \item{data}{A data.frame with four columns: cbreak,
  question, response and weight} \item{surveyorDefaults}{A
  copy of the original surveyor defaults, see
  \code{\link{surveyorDefaults}}} \item{plotTitle}{Main
  title of the plot: defaults to question text}
  \item{qid}{Question id, e.g. "Q4"}
  \item{sampleSize}{Named vector with weighted sample size
  for each crossbreak} }
}
\description{
  Creates object of class "surveyorCode".
}
\seealso{
  \code{\link{surveyPlot}} to plot a surveyor object
}

