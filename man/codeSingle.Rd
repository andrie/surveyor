\name{codeSingle}
\alias{codeSingle}
\title{Code survey data in single question form}
\usage{
  codeSingle(surveyor, q_id, wrapWidth = 50, ...)
}
\arguments{
  \item{surveyor}{Surveyor object}

  \item{q_id}{Question id}

  \item{wrapWidth}{Position where labels will be wrapped
  (in character count)}

  \item{...}{Passed to surveyorStats}
}
\value{
  data frame
}
\description{
  Code survey data in single question form (i.e. without
  subquestions)
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other codeFunctions: \code{\link{codeArray}},
  \code{\link{codeGuess}}, \code{\link{codeQuickArray}}
}
\keyword{code}

