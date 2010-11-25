################################################################################
### Define surveyor class and methods
################################################################################

#' Creates object of class surveyor
#' 
#' @param q_data data frame with survey data
#' @param q_text The question id, e.g. Q4
#' @param crossbreak Vector with crossbreak data
#' @param weight Numeric vector with weighting data
#' @param defaults Surveyor defaults
#' @export
#' @examples
#' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' s <- surveyor(q_data, q_text, crossbreak=c("aa", "bb"), weight=c(1,1)) 					
surveyor <- function(
		q_data, 
		q_text, 
		crossbreak=q_data$crossbreak,
		weight=q_data$weight,
		defaults=surveyor_defaults()
	){
	if (!identical(names(q_data), names(q_text))){
		stop("Surveyor object: The names of q_data and q_text must match")
	}
	if (length(crossbreak) != nrow(q_data)){
		stop("Surveyor object: Crossbreak must match q_data in length")
	}
	if (length(weight) != nrow(q_data)){
		stop("Surveyor object: Weight must match q_data in length")
	}
	if (!is.numeric(weight)){
		stop("Surveyor object: Weight must be numeric")
	}
	structure(
			list(
					q_data=q_data, 
					q_text=q_text,
					crossbreak=crossbreak,
					weight=weight,
					defaults=defaults
			), 
			class="surveyor"
	)
}

#' Prints surveyor object
#' 
#' @param surveyor surveyor object
print.surveyor <- function(surveyor){
	cat("Surveyor\n\n")
	print.listof(surveyor)
}

#' Tests that object is a surveyor object
#' 
#' @param x Object to be tested
#' @export
#' @examples 
#' q_data <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' q_text <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(q_text) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' s <- surveyor(q_data, q_text, crossbreak=c("aa", "bb"), weight=c(1,1))
#' is.surveyor(s) # TRUE
#' is.surveyor("String") #FALSE 					
is.surveyor <- function(x){
	if (class(x)=="surveyor"){
		if (all(
				!is.null(x$q_data),
				!is.null(x$q_text),
				!is.null(x$crossbreak),
				!is.null(x$weight),
				!is.null(x$defaults)
		)){
			TRUE
		} else {
			FALSE
		}
			
	} else {
		FALSE
	}
}



################################################################################
### Define a class of functions with a static variable (i)
### Important: This must be initialised before use
################################################################################


 ## #' Creates function and static variable used to number charts in report
 ## #' 
 ## #' Must be initialiased, e.g. ggsprint <- save_print_function
 ## #' 
 ## #' @param plot A ggplot2 plot object
 ## #' @param counter An integer, used as an index for the saved name of the plot
 ## #' @param size The print size in inches, e.g. c(4,3)
 ## #' @param dpi The print quality in dots per inch
 ## #' @param path The file path where the graphs will be saved
# save_print <- function(plot, counter, size=c(4, 3), dpi=600, path) {
#   filename <- paste("fig", counter, ".eps", sep="")
# message(paste("Now saving ", filename, sep=""))
#   ggsave(
#       plot, 
#       filename=filename, 
#       width=size[1], 
#       height=size[2],
#       dpi=dpi, 
#       path=path)
# }
	
#' Creates a counter 
#' 
#' Must be initialiased, e.g. surveyor_counter <- new_counter()
#' @export 
new_counter <- function() {
	i <- 0
	function() {
		# do something useful, then ...
		i <<- i + 1
		i
	}
}



################################################################################
### Plugin architecture to process each question
################################################################################

#' Codes and plots a survey question
#' 
#' This is the top level function that determines how a question is processed,
#' coded, printed and plotted.
#' 
#' @param surveyor Surveyor object
#' @param q_id Question id
#' @param counter An integer that defines the figure number to save to
#' @param code_function A reference to a function that processes the question data
#' @param stats_function A reference to a function that summarizes the coded data
#' @param plot_function A reference to a function that plots the summarized data
#' @param plot_size Size in inches of plot output, e.g. c(4,3)
#' @param ... Other parameters passed to code_function
#' @export
#' @seealso surveyor
plot_q <- function(
		surveyor,
		q_id,
		counter, 
		code_function = plot_single,
		stats_function = stats_bin,
		plot_function = plot_bar,
		plot_size = surveyor$defaults$default_plot_size,
		...){
	
	if(!(is.surveyor(surveyor))){
		stop("You must pass a valid surveyor object to plot_q")
	}
	
	message(q_id)
	f <- code_function(surveyor, q_id, ...)
	if (is.null(f)){
		nothing_to_plot <- TRUE
	} else {
		s <- stats_function(f)
		g <- plot_function(s, surveyor)
		nothing_to_plot <- FALSE
	}	
		
	if (!surveyor$defaults$output_to_latex){
		print(g)
	} else {
		cat(printQlatex(get_q_text(surveyor, q_id)),
				file = surveyor$defaults$output_filename, append=TRUE)
		if (nothing_to_plot){
			cat("\nNo data\n\n",
					file = surveyor$defaults$output_filename, append=TRUE)
			} else {
				if (identical(code_function, code_array)){
					plot_size[2] <- plot_size[2]*1.5
				}
				filename <- paste("fig", counter, ".eps", sep="")
				message(paste("Now saving ", filename, sep=""))
				ggsave(
						g, 
						filename = filename, 
						width    = plot_size[1], 
						height   = plot_size[2],
						dpi      = surveyor$defaults$dpi, 
						path     = surveyor$defaults$path_graphics
				)
				cat("\\PlaceGraph{graphics/", filename, "}\n", sep="",
						file = surveyor$defaults$output_filename, append=TRUE)
			}
		}
	return(invisible())
}







