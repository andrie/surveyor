\name{plot_q}
\alias{plot_q}
\title{Codes and plots a survey question...}
\usage{plot_q(surveyor, Qid, code_function, plot_function,
    plot_size=surveyor$defaults$default_plot_size, dpi=600,
    plot_path=gpath, ...)}
\description{Codes and plots a survey question}
\details{This is the top level function that determines how a question is processed,
coded, printed and plotted.}
\seealso{surveyor}
\arguments{\item{x}{Surveyor object}
\item{Qid}{Question id}
\item{code_function}{A reference to a function that processes the question data}
\item{plot_function}{A reference to a function that plots the question data}
\item{plot_size}{Size in inches of plot output, e.g. c(4,3)}
\item{dpi}{Print quality in dots per inch}
\item{plot_path}{File path where the ggplot graph is saved}
\item{...}{Other parameters passed to code_function}}
\examples{plot_q}
