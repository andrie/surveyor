\name{stats_rank}
\alias{stats_rank}
\title{Calculates summary statistics for ranking type questions...}
\usage{stats_rank(x)}
\description{Calculates summary statistics for ranking type questions}
\details{Takes the result of a code_function, e.g. code_single(), and calculates
summary values, for direct plotting by a plot_function, e.g. plot_bar()}
\value{A data frame with three columns: crossbreak, variable, value}
\arguments{\item{x}{A data frame with four columns: crossbreak, question, response, weight}}
