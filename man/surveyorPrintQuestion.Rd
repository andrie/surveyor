\name{surveyorPrintQuestion}
\alias{surveyorPrintQuestion}
\title{Prints surveyor question.}
\usage{
  surveyorPrintQuestion(surveyor, q_id, f, g, h, plotSize,
    outputType, plotMultiplierLimits = c(1, 1))
}
\arguments{
  \item{surveyor}{Surveyor object}

  \item{q_id}{Question id}

  \item{plotSize}{Size in inches of plot output, e.g.
  c(4,3)}

  \item{outputType}{"latex", "ppt" or "device": Specifies
  destination of ouput}

  \item{plotMultiplierLimits}{Numeric vector of length two,
  indicating lower and upper limit of vertical plot
  resizing}
}
\description{
  Prints surveyor question.
}
\keyword{internal}

