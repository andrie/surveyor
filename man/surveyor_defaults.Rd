\name{surveyor_defaults}
\alias{surveyor_defaults}
\title{Initialises surveyor object defaults.}
\usage{surveyor_defaults(output_to_latex=TRUE, default_theme_size=9,
    question_pattern="_[[:digit:]]*$", subquestion_append=TRUE,
    subquestion_prepend=!subquestion_append, fastgraphics=FALSE,
    default_colour_area=rgb(127, 201, 127, 255, maxColorValue = 255),
    default_colour_point=rgb(27, 158, 119, 255, maxColorValue = 255))}
\description{Initialises surveyor object defaults.}
\seealso{\code{\link{as.surveyor}}}
\arguments{\item{output_to_latex}{TRUE or FALSE, determines if latex commands is output}
\item{default_theme_size}{Text size in points, passed to ggplot}
\item{question_pattern}{A text pattern passed to grep() to distinguish 
between single and array questions}
\item{subquestion_append}{Indicates whether subquestion text is appended to question text}
\item{subquestion_prepend}{Indicates whether subquestion text is prepended to question text}
\item{fastgraphics}{Uses lattice graphics if true, otherwise ggplot}
\item{default_colour_area}{Default RGB colour for areas in graphs (e.g. bars)}
\item{default_colour_point}{Default RGB colour for points in graphs (e.g. points)}}
\examples{s <- surveyor_defaults()
s <- surveyor_defaults(output_to_latex=TRUE)}
