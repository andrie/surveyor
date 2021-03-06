\name{codeQuickArray}
\alias{codeQuickArray}
\title{Code survey data in single or array question form.}
\usage{
  codeQuickArray(surveyor, q_id,
    crossbreak = surveyor$crossbreak[[1]], wrapWidth = 50,
    ...)
}
\arguments{
  \item{surveyor}{Surveyor object}

  \item{q_id}{Question id}

  \item{crossbreak}{Crossbreak vector}

  \item{wrapWidth}{Position where labels will be wrapped.
  Passed to \code{\link[stringr]{str_wrap}}}

  \item{...}{Other parameters passed on to downstream code*
  functions}
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

  Other codeFunctions: \code{\link{codeGuess}}
}
\keyword{code}

