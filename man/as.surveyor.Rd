\name{as.surveyor}
\alias{as.surveyor}
\title{Creates object of class surveyor.}
\usage{as.surveyor(q_data, q_text, crossbreak=q_data$crossbreak,
    weight=q_data$weight, defaults=surveyor_defaults())}
\description{Creates object of class surveyor.}
\details{A surveyor object describes the data and meta-data in the survey that will be analysed by the analysis and reporting functions in the surveyor package.}
\value{A list object of class surveyor}
\arguments{\item{q_data}{data frame with survey data}
\item{q_text}{The question metadata that contains the original text question}
\item{crossbreak}{List of factors that contain crossbreak data.}
\item{weight}{Numeric vector with weighting data}
\item{defaults}{Surveyor defaults.  See also \code{\link{surveyor_defaults}}}}
\examples{q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
s <- as.surveyor(q_data, q_text, crossbreak=c("aa", "bb"), weight=c(1,1))}
