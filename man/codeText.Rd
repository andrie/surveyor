\name{codeText}
\alias{codeText}
\title{Code survey data in text question form (i.e. open-ended text}
\usage{
  codeText(surveyor, q_id, inn = NULL, out = NULL,
  filter_responses = TRUE)
}
\arguments{
  \item{surveyor}{Surveyor object}

  \item{q_id}{Question id}

  \item{inn}{string passed to latexTranslate()}

  \item{out}{string passed to latexTranslate()}

  \item{filter_responses}{Indicates whether responses
  should be filtered first}
}
\value{
  data frame
}
\description{
  Code survey data in text question form (i.e. open-ended
  text
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other codeFunctions: \code{\link{codeArray}},
  \code{\link{codeGuess}}, \code{\link{codeQuickArray}},
  \code{\link{codeSingle}}
}
\keyword{code}

