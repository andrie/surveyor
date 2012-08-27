\name{surveyTableApply}
\alias{surveyTableApply}
\title{Apply surveyTable to list of question definitions.}
\usage{
  surveyTableApply(banner, s, mlist, simplify = TRUE,
    remove = c("statsText"), allBreaks = TRUE)
}
\arguments{
  \item{banner}{a list of arguments to send to
  \code{\link{surveyPlot}}}

  \item{s}{Optional surveyor object. If supplied, appends s
  to each element of defs}

  \item{mlist}{Options list of modified arguments}

  \item{simplify}{If TRUE, simplifies results to data
  frame, otherwise the result is a list}

  \item{remove}{a character vector of stats_methods to
  remove from data}

  \item{allBreaks}{If TRUE, sets all \code{onlyBreaks}
  arguments to NULL, thus creating surveyTable for all
  crossbreaks in surveyor}
}
\value{
  A data frame (if simplified=TRUE), otherwise a list
}
\description{
  Apply surveyTable to list of question definitions.
}
\seealso{
  Other Surveyor apply functions:
  \code{\link{surveyPlotApply}}
}

