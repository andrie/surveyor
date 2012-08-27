\name{str.surveyor}
\alias{str.surveyor}
\title{Display the structure of a surveyor object.}
\usage{
  \method{str}{surveyor} (object, list.len = 10, ...)
}
\arguments{
  \item{object}{Surveyor object}

  \item{list.len}{Numeric. Maximum number of list elements
  to display within a level. Passed to
  `\code{\link[utils]{str}}}

  \item{...}{Other arguments passed to
  \code{\link[utils]{str}}}
}
\description{
  Method for \code{\link[utils]{str}} to display surveyor
  object. The only modification is to limit the list length
  to display, thus de-emphasizing the \code{surveydata}
  element.
}

