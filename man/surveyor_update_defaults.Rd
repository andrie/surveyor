\name{surveyor_update_defaults}
\alias{surveyor_update_defaults}
\title{Selectively updates surveyor defaults.}
\usage{
  surveyor_update_defaults(surveyor, output_to_latex =
  NULL, default_theme_size = NULL, question_pattern = NULL,
  subquestion_append = NULL, subquestion_prepend = NULL,
  fastgraphics = NULL, add_plot_title = NULL,
  default_colour_area = NULL, default_colour_point = NULL,
  print_table = NULL)
}
\arguments{
  \item{surveyor}{Surveyor object}

  \item{output_to_latex}{TRUE or FALSE, determines if latex
  commands is output}

  \item{default_theme_size}{Text size in points, passed to
  ggplot}

  \item{question_pattern}{A text pattern passed to grep()
  to distinguish between single and array questions}

  \item{subquestion_append}{Indicates whether subquestion
  text is appended to question text}

  \item{subquestion_prepend}{Indicates whether subquestion
  text is prepended to question text}

  \item{fastgraphics}{Uses lattice graphics if true,
  otherwise ggplot}

  \item{add_plot_title}{If true, adds question text as plot
  title}

  \item{default_colour_area}{Default RGB colour for areas
  in graphs (e.g. bars)}

  \item{default_colour_point}{Default RGB colour for points
  in graphs (e.g. points)}

  \item{print_table}{If TRUE will print the table as part
  of the report}
}
\description{
  Selectively updates surveyor defaults.
}
\seealso{
  \code{\link{as.surveyor}},
  \code{\link{surveyor_defaults}}
}

