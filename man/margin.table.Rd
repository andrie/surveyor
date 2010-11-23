\name{margin.table}
\alias{margin.table}
\title{For a contingency table in array form, compute the sum of table entries for a given index.}
\usage{margin.table(x, margin, na.rm=TRUE)}
\description{For a contingency table in array form, compute the sum of table entries for a given index.}
\details{Redefines margin.table to deal with NA values}
\arguments{\item{x}{an array}
\item{margin}{index number (1 for rows, etc.)}
\item{na.rm}{logical. Should missing values be removed? Passed to sum()
#'}}
\examples{#' m <- matrix(1:4,2)
#' surveyor:::margin.table(m, 1)
#' surveyor:::margin.table(m, 2)}
