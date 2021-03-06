\name{is.surveyor}
\alias{is.surveyor}
\title{Tests that object is of class surveyor object.}
\usage{
  is.surveyor(x)
}
\arguments{
  \item{x}{Object to be tested}
}
\description{
  Tests that object is of class surveyor object.
}
\examples{
library(surveydata)
sdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
varlabels(sdata) <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
sdata <- as.surveydata(sdata, renameVarlabels=TRUE)
s <- as.surveyor(sdata, crossbreak=list(breaks=factor(c("aa", "bb"))), weight=c(1,1))
is.surveyor(s) # TRUE
is.surveyor("String") #FALSE
}

