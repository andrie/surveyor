\name{as.surveyor}
\alias{as.surveyor}
\title{Creates object of class surveyor.}
\usage{
  as.surveyor(sdata, crossbreak = sdata$crossbreak,
    weight = sdata$weight, defaults = surveyorDefaults())
}
\arguments{
  \item{sdata}{surveydata object. See also
  \code{\link[surveydata]{as.surveydata}}}

  \item{crossbreak}{List of named factors that contain
  crossbreak data. Each factor must have the same length as
  the columns in the \code{surveydata} object}

  \item{weight}{Numeric vector with weighting data. Must
  have the same length as the columns in the
  \code{surveydata} object}

  \item{defaults}{Surveyor defaults.  See also
  \code{\link{surveyorDefaults}}}
}
\value{
  A list object of class \code{surveyor}: \describe{
  \item{sdata}{surveydata object. See also
  \code{\link[surveydata]{as.surveydata}}} \item{cbreak}{}
  \item{plot_title}{} \item{weight}{} \item{defaults}{} }
}
\description{
  A surveyor object describes the data and meta-data in the
  survey that will be analysed by the analysis and
  reporting functions in the surveyor package.
}
\examples{
library(surveydata)
sdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
varlabels(sdata) <- qtext
sdata <- as.surveydata(sdata, renameVarlabels=TRUE)
s <- as.surveyor(sdata, crossbreak=list(breaks=factor(c("aa", "bb"))), weight=c(1,1))
}

