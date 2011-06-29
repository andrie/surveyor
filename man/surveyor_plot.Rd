\name{surveyor_plot}
\alias{surveyor_plot}
\title{Codes and plots a survey question...}
\usage{surveyor_plot(surveyor, q_id, code_function=code_guess,
    stats_function=stats_guess, plot_function=plot_guess,
    plot_size=surveyor$braid$default_plot_size,
    output_to_latex=surveyor$defaults$output_to_latex, ...)}
\description{Codes and plots a survey question}
\details{This is the top level function that determines how a question is processed,
coded, printed and plotted.}
\seealso{\code{\link{as.surveyor}}}
\arguments{\item{surveyor}{Surveyor object}
\item{q_id}{Question id}
\item{code_function}{A reference to a function that processes the question data}
\item{stats_function}{A reference to a function that summarizes the coded data}
\item{plot_function}{A reference to a function that plots the summarized data}
\item{plot_size}{Size in inches of plot output, e.g. c(4,3)}
\item{output_to_latex}{If TRUE, wraps output in latex code, otherwise print to console}
\item{...}{Other parameters passed to code_function}}
