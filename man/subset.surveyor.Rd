\name{subset.surveyor}
\alias{subset.surveyor}
\title{Subsetting surveyor objects.}
\usage{
  \method{subset}{surveyor} (x, subset, ...)
}
\arguments{
  \item{x}{Surveyor object}

  \item{subset}{Subset to evaluate. This is evaluated in
  the environment of the surveyor data, i.e.
  \code{x$sdata}}

  \item{...}{Ignored}
}
\value{
  surveyor object
}
\description{
  Returns subset of surveyor object by applying the subset
  to the data, weight as well as crossbreak elements of the
  surveyor object.
}

