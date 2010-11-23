\name{get_q_subquestions}
\alias{get_q_subquestions}
\title{Identifies subquestions of a given question in a data...}
\usage{get_q_subquestions(Qdata, Qid)}
\description{Identifies subquestions of a given question in a data.frame}
\details{Given a question id, e.g. Q4, finds all subquestions, e.g. Q4_1, Q4_2, etc.}
\arguments{\item{Qdata}{Qdata frame with survey Qdata}
\item{Qid}{The question id, e.g. Q4
#'}}
\examples{#' qdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(qtext) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' surveyor:::get_q_subquestions(qdata, "Q4")}
