\name{identify_net_score}
\alias{identify_net_score}
\title{Guesses whether a question should be coded as net score.}
\usage{identify_net_score(x, match_words)}
\description{Guesses whether a question should be coded as net score.}
\details{Evaluates the first and last factor levels of x and tests whether these levels contain words in match_words}
\value{A data frame with three columns - cbreak, variable, value}
\keyword{internal}
\arguments{\item{x}{A factor of character strings}
\item{match_words}{A character vector of words to match}}
