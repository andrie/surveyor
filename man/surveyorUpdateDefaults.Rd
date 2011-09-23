\name{surveyorUpdateDefaults}
\alias{surveyorUpdateDefaults}
\title{Selectively updates surveyor defaults.}
\usage{
  surveyorUpdateDefaults(surveyor, outputToLatex = NULL,
  defaultThemeSize = NULL, questionPattern = NULL,
  subquestionAppend = NULL, subquestionPrepend = NULL,
  fastgraphics = NULL, addPlotTitle = NULL,
  defaultColourArea = NULL, defaultColourPoint = NULL,
  printTable = NULL)
}
\arguments{
  \item{surveyor}{Surveyor object}

  \item{outputToLatex}{TRUE or FALSE, determines if latex
  commands is output}

  \item{defaultThemeSize}{Text size in points, passed to
  ggplot}

  \item{questionPattern}{A text pattern passed to grep() to
  distinguish between single and array questions}

  \item{subquestionAppend}{Indicates whether subquestion
  text is appended to question text}

  \item{subquestionPrepend}{Indicates whether subquestion
  text is prepended to question text}

  \item{fastgraphics}{Uses lattice graphics if true,
  otherwise ggplot}

  \item{addPlotTitle}{If true, adds question text as plot
  title}

  \item{defaultColourArea}{Default RGB colour for areas in
  graphs (e.g. bars)}

  \item{defaultColourPoint}{Default RGB colour for points
  in graphs (e.g. points)}

  \item{printTable}{If TRUE will print the table as part of
  the report}
}
\description{
  Selectively updates surveyor defaults.
}
\seealso{
  \code{\link{as.surveyor}}, \code{\link{surveyorDefaults}}
}

