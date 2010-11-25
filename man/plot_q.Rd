\name{plot_q}
\alias{plot_q}
\title{Codes and plots a survey question...}
\usage{plot_q(surveyor, q_id, counter, code_function=plot_single,
    stats_function=stats_bin, plot_function=plot_bar,
    plot_size=surveyor$defaults$default_plot_size, ...)}
\description{Codes and plots a survey question}
\details{This is the top level function that determines how a question is processed,
coded, printed and plotted.}
\seealso{surveyor}
\arguments{\item{surveyor}{Surveyor object}
\item{q_id}{Question id}
\item{counter}{An integer that defines the figure number to save to}
\item{code_function}{A reference to a function that processes the question data}
\item{stats_function}{A reference to a function that summarizes the coded data}
\item{plot_function}{A reference to a function that plots the summarized data}
\item{plot_size}{Size in inches of plot output, e.g. c(4,3)}
\item{...}{Other parameters passed to code_function}}
