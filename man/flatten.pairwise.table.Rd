\name{flatten.pairwise.table}
\alias{flatten.pairwise.table}
\title{Takes pairwise comparison table and flatten it for easy printing.}
\usage{
  flatten.pairwise.table(x)
}
\arguments{
  \item{x}{A matrix, typically the result of
  \code{\link[stats]{pairwise.table}}}
}
\description{
  Takes pairwise comparison table and flatten it for easy
  printing.
}
\examples{
data(airquality)
airquality <- within(airquality, Month <- factor(Month, labels = month.abb[5:9]))
x <- with(airquality, pairwise.t.test(Ozone, Month))
surveyor:::flatten.pairwise.table(x$p.value)
}

