\name{plotGuess}
\alias{plotGuess}
\title{Guesses which plot format is optimal}
\usage{
  plotGuess(s, ...)
}
\arguments{
  \item{s}{A surveyorStats object}

  \item{...}{Other parameters passed to specific plot
  function}
}
\description{
  Investigates columns in supplied data, and then chooses
  either \code{\link{plotBar}} or \code{\link{plotColumn}}
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other plotFunctions: \code{\link{plotBar}},
  \code{\link{plotBarSum}}, \code{\link{plotColumn}},
  \code{\link{plotNetScore}}, \code{\link{plotNull}},
  \code{\link{plotPoint}}, \code{\link{plotText}}
}
\keyword{plot}

