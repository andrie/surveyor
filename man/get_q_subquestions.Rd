\name{get_q_subquestions}
\alias{get_q_subquestions}
\title{Identifies subquestions of a given question in a data frame.}
\usage{
  get_q_subquestions(q_data, q_id, surveyor = NULL)
}
\arguments{
  \item{q_data}{data frame with survey data}

  \item{q_id}{The question id, e.g. Q4}

  \item{surveyor}{surveyor object used to set default
  question pattern}
}
\description{
  Given a question id, e.g. Q4, finds all subquestions,
  e.g. Q4_1, Q4_2, etc. The pattern can be set by modifying
  question_pattern in the surveyor defaults
}
\examples{
q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
get_q_subquestions(q_data, "Q4")
}

