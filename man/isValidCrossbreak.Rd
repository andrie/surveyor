\name{isValidCrossbreak}
\alias{isValidCrossbreak}
\title{Tests that crossbreak is valid.}
\usage{
  isValidCrossbreak(x, surveyorObject,
    nrow = nrow(surveyorObject$sdata), throwErrors = TRUE)
}
\arguments{
  \item{x}{A crossbreak object to test}

  \item{surveyorObject}{A surveyor object}

  \item{nrow}{The required length of the crossbreak, i.e.
  the number of rows in the surveydata object}

  \item{throwErrors}{Logical. If TRUE, throws errors, else
  simply returns TRUE or FALSE}
}
\description{
  A crossbreak object must be a list, and each element must
  be a named vector
}
\keyword{internal}

