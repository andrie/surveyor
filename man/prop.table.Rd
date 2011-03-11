\name{prop.table}
\alias{prop.table}
\title{Express Table Entries as Fraction of Marginal Table...}
\usage{prop.table(x, margin, na.rm=TRUE)}
\description{Express Table Entries as Fraction of Marginal Table}
\details{Redefines prop.table to deal with NA values}
\arguments{\item{x}{an array}
\item{margin}{index number (1 for rows, etc.)}
\item{na.rm}{logical. Should missing values be removed? Passed to sum()}}
\examples{m <- matrix(1:4,2)
prop.table(m,1)
prop.table(m,2)}
