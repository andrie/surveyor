\name{plotText}
\alias{plotText}
\title{Plot data as text.}
\usage{
  plotText(s, plotFunction = "plotText",
    textOutput = c("latex", "text"), ...)
}
\arguments{
  \item{s}{A surveyorStats object}

  \item{plotFunction}{Character vector: Identifies the name
  of the plot function used to create the plot}

  \item{textOutput}{Use \code{latex} for latex ouput, or
  \code{text} for normal text ouput}

  \item{...}{Ignored}
}
\description{
  Plots questions that are summarised using
  \code{\link{statsText}}.
}
\seealso{
  For an overview of the surveyor package
  \code{\link{surveyor}}

  Other plotFunctions: \code{\link{plotBar}},
  \code{\link{plotBarSum}}, \code{\link{plotColumn}},
  \code{\link{plotGuess}}, \code{\link{plotNetScore}},
  \code{\link{plotNull}}, \code{\link{plotPoint}}
}
\keyword{plot}

