\name{.sprop.table}
\alias{.sprop.table}
\title{Express Table Entries as Fraction of Marginal Table...}
\usage{.sprop.table(x, margin, na.rm=TRUE)}
\description{Express Table Entries as Fraction of Marginal Table}
\details{Redefines prop.table to deal with NA values}
\keyword{internal}
\arguments{\item{x}{an array}
\item{margin}{index number (1 for rows, etc.)}
\item{na.rm}{logical. Should missing values be removed? Passed to sum()}}
\examples{m <- matrix(1:4,2)
surveyor:::.sprop.table(m,1)
surveyor:::.sprop.table(m,2)}
