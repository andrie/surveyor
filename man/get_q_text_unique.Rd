\name{get_q_text_unique}
\alias{get_q_text_unique}
\title{Returns unique elements of question text.}
\usage{get_q_text_unique(q_data, q_id, q_text, surveyor)}
\description{Returns unique elements of question text.}
\details{Given a question id, e.g. Q4, finds all subquestions, e.g. Q4_1, Q4_2, etc, 
and returns the question text that is unique to each}
\seealso{\code{\link{get_q_text}}, \code{\link{get_q_text_common}}}
\arguments{\item{q_data}{data frame with survey data}
\item{q_id}{string containing the question id, e.g. Q4. This has to
match a name in q_data}
\item{q_text}{Named character vector containing the question text}
\item{surveyor}{surveyor object, used to set default question pattern}}
\examples{q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
get_q_text_unique(q_data, "Q4", q_text)}
