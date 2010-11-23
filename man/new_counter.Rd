\name{new_counter}
\alias{new_counter}
\title{#' Creates function and static variable used to number charts in report...}
\description{#' Creates function and static variable used to number charts in report
#' 
#' Must be initialiased, e.g. ggsprint <- save_print_function
#' 
#'}
\arguments{\item{plot}{A ggplot2 plot object
#'}
\item{counter}{An integer, used as an index for the saved name of the plot
#'}
\item{size}{The print size in inches, e.g. c(4,3)
#'}
\item{dpi}{The print quality in dots per inch
#'}
\item{path}{The file path where the graphs will be saved
Creates a counter 

Must be initialiased, e.g. surveyor_counter <- new_counter()}}
