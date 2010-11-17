\name{get_qtext_common}
\alias{get_qtext_common}
\title{Returns common elements of question text...}
\usage{get_qtext_common(Qdata, Qid, Qtext)}
\description{Returns common elements of question text}
\details{Given a question id, e.g. Q4, finds all subquestions, e.g. Q4_1, Q4_2, etc, 
and returns the question text that is common to all}
\seealso{get_qtext, get_qtext_unique}
\arguments{\item{Qdata}{Qdata frame with survey Qdata}
\item{Qid}{The question id, e.g. Q4}
\item{Qtext}{Named character vector containing the question text}}
\examples{qdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
names(qtext) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
get_qtext_common(qdata, "Q4", qtext)}
