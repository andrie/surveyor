\name{as.surveyor}
\alias{as.surveyor}
\title{Creates object of class surveyor.}
\usage{
  as.surveyor(sdata, crossbreak = sdata$crossbreak, weight
  = sdata$weight, defaults = surveyorDefaults(), braid =
  as.braid())
}
\arguments{
  \item{sdata}{surveydata object}

  \item{crossbreak}{List of factors that contain crossbreak
  data.}

  \item{weight}{Numeric vector with weighting data}

  \item{defaults}{Surveyor defaults.  See also
  \code{\link{surveyorDefaults}}}

  \item{braid}{A braid object}
}
\value{
  A list object of class surveyor
}
\description{
  A surveyor object describes the data and meta-data in the
  survey that will be analysed by the analysis and
  reporting functions in the surveyor package.
}
\examples{
sdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
varlabels(sdata) <- qtext
sdata <- as.surveydata(sdata)
b <- as.braid()
s <- as.surveyor(sdata, crossbreak=c("aa", "bb"), weight=c(1,1), braid=b)
}

