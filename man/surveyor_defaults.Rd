\name{surveyor_defaults}
\alias{surveyor_defaults}
\title{Initialises surveyor object defaults...}
\usage{surveyor_defaults(path_latex=Sys.getenv()["TEMP"],
    path_graphics=path_latex, output_to_latex=FALSE,
    output_filename=file.path(path_latex, "surveyor.tex"),
    default_theme_size=12, default_plot_size=c(5, 3),
    default_colour_area=rgb(127, 201, 127, 255, maxColorValue = 255),
    default_colour_point=rgb(27, 158, 119, 255, maxColorValue = 255),
    dpi=600)}
\description{Initialises surveyor object defaults}
\seealso{surveyor}
\arguments{\item{path_latex}{Path where latex will be saved}
\item{path_graphics}{Path where graphics files will be saved}
\item{output_to_latex}{TRUE or FALSE, determines if latex commands is output}
\item{output_filename}{Filename where latex output will be saved}
\item{default_theme_size}{Text size in points, passed to ggplot}
\item{default_plot_size}{Plot size in inches, e.g. c(4, 3)}
\item{default_colour_area}{Default RGB colour for areas in graphs (e.g. bars)}
\item{default_colour_point}{Default RGB colour for points in graphs (e.g. points)}
\item{dpi}{Dots per inch, passed to ggsave()}}
\examples{s <- surveyor_defaults()
s <- surveyor_defaults(output_to_latex=TRUE)}
