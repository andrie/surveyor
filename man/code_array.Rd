\name{code_array}
\alias{code_array}
\title{Code survey data in array question form}
\usage{
  code_array(surveyor, q_id, remove_other = FALSE,
  wrap_length = 50)
}
\arguments{
  \item{surveyor}{Surveyor object}

  \item{q_id}{Question id}

  \item{remove_other}{If true, will remove last column}

  \item{wrap_length}{Position where labels will be wrapped
  (in character count)}
}
\value{
  data frame
}
\description{
  Code survey data in array question form (i.e. with
  subquestions)
}
\seealso{
  Other coding functions: \code{\link{code_single}},
  \code{\link{code_array}}, \code{\link{code_text}},
  \code{\link{code_guess}}

  For an overview of the surveyor package
  \code{\link{surveyor}}
}
\keyword{code}

