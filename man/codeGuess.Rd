\name{codeGuess}
\alias{codeGuess}
\title{Code survey data in single or array question form.}
\usage{
  codeGuess(surveyor, q_id, ...)
}
\arguments{
  \item{surveyor}{Surveyor object}

  \item{q_id}{Question id}

  \item{...}{Other parameters passed on to downstream
  code_* functions}
}
\value{
  data frame
}
\description{
  Code survey data in array question form (i.e. with
  subquestions)
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other codeFunctions: \code{\link{codeQuickArray}}

  Other surveyPlot arguments:
  \code{\link{as.surveyorPlot}}, \code{\link{statsGuess}}
}
\keyword{code}

