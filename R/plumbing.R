################################################################################
### Define surveyor class and methods
################################################################################

#' Creates object of class surveyor
#' 
#' @param qdata data frame with survey data
#' @param qtext The question id, e.g. Q4
#' @param crossbreak Vector with crossbreak data
#' @param weight Numeric vector with weighting data
#' @param defaults Surveyor defaults
#' @export
#' @examples
#' qdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(qtext) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' s <- surveyor(qdata, qtext, crossbreak=c("aa", "bb"), weight=c(1,1)) 					
surveyor <- function(
		qdata, 
		qtext, 
		crossbreak=qdata$crossbreak,
		weight=qdata$weight,
		defaults=surveyor_defaults()
	){
	if (!identical(names(qdata), names(qtext))){
		stop("Surveyor object: The names of qdata and qtext must match")
	}
	if (length(crossbreak) != nrow(qdata)){
		stop("Surveyor object: Crossbreak must match qdata in length")
	}
	if (length(weight) != nrow(qdata)){
		stop("Surveyor object: Weight must match qdata in length")
	}
	if (!is.numeric(weight)){
		stop("Surveyor object: Weight must be numeric")
	}
	structure(
			list(
					qdata=qdata, 
					qtext=qtext,
					crossbreak=crossbreak,
					weight=weight,
					defaults=defaults
			), 
			class="surveyor"
	)
}

#' Tests that object is a surveyor object
#' 
#' @param x Object to be tested
#' @export
#' @examples 
#' qdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(qtext) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' s <- surveyor(qdata, qtext, crossbreak=c("aa", "bb"), weight=c(1,1))
#' is.surveyor(s) # TRUE
#' is.surveyor("String") #FALSE 					
is.surveyor <- function(x){
	if (class(x)=="surveyor"){
		if (all(
				!is.null(x$qdata),
				!is.null(x$qtext),
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
#' @param Qid Question id
#' @param counter An integer that defines the figure number to save to
#' @param code_function A reference to a function that processes the question data
#' @param plot_function A reference to a function that plots the question data
#' @param plot_size Size in inches of plot output, e.g. c(4,3)
#' @param ... Other parameters passed to code_function
#' @export
#' @seealso surveyor
plot_q <- function(
		surveyor,
		Qid,
		counter, 
		code_function,
		plot_function,
		plot_size = surveyor$defaults$default_plot_size,
		...){
	
	if(!(is.surveyor(surveyor))){
		stop("You must pass a valid surveyor object to plot_q")
	}
	
	message(Qid)
	f <- code_function(surveyor, Qid, ...)
	g <- plot_function(f, surveyor)
	if (identical(code_function, code_array)){
		plot_size[2] <- plot_size[2]*1.5
	}
	
	if (surveyor$defaults$output_to_latex){
		filename <- paste("fig", counter, ".eps", sep="")
		message(paste("Now saving ", filename, sep=""))
		ggsave(
				g, 
				filename = filename, 
				width    = plot_size[1], 
				height   = plot_size[2],
				dpi      = surveyor$defaults$dpi, 
				path     = surveyor$defaults$path_graphics)
		cat(printQlatex(get_qtext(surveyor, Qid)),
				file = surveyor$defaults$output_filename, append=TRUE)
		cat("\\PlaceGraph{graphics/", filename, "}\n", sep="",
			  file = surveyor$defaults$output_filename, append=TRUE)

	} else {
		print(g)
	}
	
	return(invisible())
}






