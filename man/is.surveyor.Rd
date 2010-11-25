\name{is.surveyor}
\alias{is.surveyor}
\title{Tests that object is a surveyor object...}
\usage{is.surveyor(x)}
\description{Tests that object is a surveyor object}
\arguments{\item{x}{Object to be tested}}
\examples{q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
s <- surveyor(q_data, q_text, crossbreak=c("aa", "bb"), weight=c(1,1))
is.surveyor(s) # TRUE
is.surveyor("String") #FALSE}
