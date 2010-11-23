\name{get_qtext}
\alias{get_qtext}
\title{Returns question text...}
\usage{get_qtext(surveyor, Qid)}
\description{Returns question text}
\details{Given a question id, e.g. "Q4", returns question text}
\seealso{get_qtext_common, get_qtext_unique
#'}
\arguments{\item{surveyor}{A surveyor object}
\item{Qid}{The question id, e.g. Q4}}
\examples{#' qdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(qtext) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' surveyor:::get_qtext(qdata, "Q1", qtext)
#'' surveyor:::get_qtext(qdata, "Q4", qtext)}
