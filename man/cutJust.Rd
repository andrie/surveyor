\name{cutJust}
\alias{cutJust}
\title{Cuts values and assign new values to each break.}
\usage{
  cutJust(x, breaks, newValues)
}
\arguments{
  \item{x}{Vector to cut}

  \item{breaks}{Vector with break points.  Needs to include
  the min and max of x}

  \item{newValues}{Vector with new values. Should be length
  one less than breaks.}
}
\description{
  This is useful for creating hjust and vjust positions for
  text on plots.  this is essentially a wrapper around
  \code{\link{cut}}
}

