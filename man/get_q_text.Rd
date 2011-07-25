\name{get_q_text}
\alias{get_q_text}
\title{Returns question text}
\usage{
  get_q_text(surveyor, q_id)
}
\arguments{
  \item{surveyor}{A surveyor object}

  \item{q_id}{The question id, e.g. Q4}
}
\description{
  Given a question id, e.g. "Q4", returns question text
}
\examples{
q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
get_q_text(q_data, "Q1")
get_q_text(q_data, "Q4")
}
\seealso{
  get_q_text_common, get_q_text_unique
}

