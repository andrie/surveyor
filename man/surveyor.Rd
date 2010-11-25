\name{surveyor}
\alias{surveyor}
\title{Creates object of class surveyor...}
\usage{surveyor(q_data, q_text, crossbreak=q_data$crossbreak,
    weight=q_data$weight, defaults=surveyor_defaults())}
\description{Creates object of class surveyor}
\arguments{\item{q_data}{data frame with survey data}
\item{q_text}{The question id, e.g. Q4}
\item{crossbreak}{Vector with crossbreak data}
\item{weight}{Numeric vector with weighting data}
\item{defaults}{Surveyor defaults}}
\examples{q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
s <- surveyor(q_data, q_text, crossbreak=c("aa", "bb"), weight=c(1,1))}
