\name{identify_netScore}
\alias{identify_netScore}
\title{Guesses whether a question should be coded as net score.}
\usage{
  identify_netScore(x, match_words = NULL)
}
\arguments{
  \item{x}{A factor of character strings}

  \item{match_words}{A character vector of words to match}
}
\value{
  A data frame with three columns - cbreak, variable, value
}
\description{
  Evaluates the first and last factor levels of x and tests
  whether these levels contain words in match_words
}
\keyword{internal}

