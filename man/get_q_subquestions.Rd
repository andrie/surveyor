\name{get_q_subquestions}
\alias{get_q_subquestions}
\title{Identifies subquestions of a given question in a data...}
\usage{get_q_subquestions(q_data, q_id)}
\description{Identifies subquestions of a given question in a data.frame}
\details{Given a question id, e.g. Q4, finds all subquestions, e.g. Q4_1, Q4_2, etc.}
\arguments{\item{q_data}{q_data frame with survey q_data}
\item{q_id}{The question id, e.g. Q4
#'}}
\examples{#' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' surveyor:::get_q_subquestions(q_data, "Q4")}
