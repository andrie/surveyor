\name{code_multi2}
\alias{code_multi2}
\title{Code survey data in multiple question form 2...}
\usage{code_multi2(surveyor, q_id, remove_other=FALSE)}
\description{Code survey data in multiple question form 2}
\details{Code survey data in multiple question form (i.e. with subquestions)}
\value{data frame}
\seealso{Coding functions:
\code{\link{code_single}}, 
\code{\link{code_array}},
\code{\link{code_text}}
Summarising functions:
\code{\link{stats_bin}}, 
\code{\link{stats_net_score}}
Plot functions: 
\code{\link{plot_bar}}, 
\code{\link{plot_point}}}
\arguments{\item{surveyor}{Surveyor object}
\item{q_id}{Question id}
\item{remove_other}{If true, will remove last column}}
