\name{codeArray}
\alias{codeArray}
\title{Code survey data in array question form}
\usage{
  codeArray(surveyor, q_id, remove_other = FALSE, wrapWidth
  = 50, ...)
}
\arguments{
  \item{surveyor}{Surveyor object}

  \item{q_id}{Question id}

  \item{remove_other}{If true, will remove last column}

  \item{wrapWidth}{Position where labels will be wrapped
  (in character count)}

  \item{...}{Passed to surveyorStats}
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

  Other codeFunctions: \code{\link{codeGuess}},
  \code{\link{codeQuickArray}}, \code{\link{codeSingle}}
}
\keyword{code}

