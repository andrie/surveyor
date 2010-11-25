\name{code_multi}
\alias{code_multi}
\title{Code survey data in multiple question form...}
\usage{code_multi(surveyor, q_id, remove_other=FALSE)}
\description{Code survey data in multiple question form}
\details{Code survey data in multiple question form (i.e. with subquestions)}
\value{data frame}
\seealso{Coding functions:
\code{\link{code_single}}, 
\code{\link{code_array}},
\code{\link{code_rank}},
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
