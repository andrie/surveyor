\name{get_q_text_common}
\alias{get_q_text_common}
\title{Returns common elements of question text...}
\usage{get_q_text_common(q_data, q_id, q_text, surveyor)}
\description{Returns common elements of question text}
\details{Given a question id, e.g. Q4, finds all subquestions, e.g. Q4_1, Q4_2, etc, 
and returns the question text that is common to all}
\seealso{get_q_text, get_q_text_unique
#'}
\arguments{\item{q_data}{q_data frame with survey q_data}
\item{q_id}{The question id, e.g. Q4}
\item{q_text}{Named character vector containing the question text}
\item{surveyor}{surveyor object, used to set default question pattern}}
\examples{#' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' surveyor:::get_q_text_common(q_data, "Q4", q_text)}
