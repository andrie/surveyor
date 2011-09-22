\name{surveyorDefaults}
\alias{surveyorDefaults}
\title{Initialises surveyor object defaults.}
\usage{
  surveyorDefaults(outputToLatex = TRUE, defaultThemeSize =
  9, questionPattern = "_[[:digit:]]*$", subquestionAppend
  = TRUE, subquestionPrepend = !subquestionAppend,
  fastgraphics = FALSE, addPlotTitle = FALSE,
  defaultColourArea = rgb(127, 201, 127, 255, maxColorValue
  = 255), defaultColourPoint = rgb(27, 158, 119, 255,
  maxColorValue = 255), printTable = TRUE)
}
\arguments{
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
  Initialises surveyor object defaults.
}
\examples{
s <- surveyorDefaults()
s <- surveyorDefaults(outputToLatex=TRUE)
}
\seealso{
  \code{\link{as.surveyor}},
  \code{\link{surveyorUpdateDefaults}}
}

