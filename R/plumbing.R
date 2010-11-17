################################################################################
### Define surveyor class and methods
################################################################################

#' Creates object of class surveyor
#' 
#' @param qdata data frame with survey data
#' @param qtext The question id, e.g. Q4
#' @param defaults
#' @export
#' @examples
#' qdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(qtext) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' surveyor(qdata, qtext) 					
surveyor <- function(qdata, qtext, defaults=surveyor_defaults()){
	if (identical(names(qdata), names(qtext))){
		structure(
				list(
						qdata=qdata, 
						qtext=qtext,
						defaults=defaults
				), 
				class="surveyor"
		)
	} else {
		stop("The names of qdata and qtext must match")
	}	
}

#' Tests that object is a surveyor object
#' 
#' @param x Object to be tested
#' @examples 
#' qdata <- data.frame(Q1=c(11, 12), Q4_1 = c(1,2), Q4_2=c(3,4), Q4_3=c(5,6))
#' qtext <- c("Question 1", "Question 4: red", "Question 4: yellow", "Question 4: blue")
#' names(qtext) <- c("Q1", "Q4_1", "Q4_2", "Q4_3")
#' s <- surveyor(qdata, qtext)
#' is.surveyor(s) # TRUE
#' is.surveyor("String") #FALSE 					
is.surveyor <- function(x){
	if (all(
			class(x)=="surveyor",
			!is.null(x$qdata),
			!is.null(x$qtext),
			!is.null(x$defaults))
		) {
		TRUE
	} else {
		FALSE
	}
}



################################################################################
### Define a class of functions with a static variable (i)
### Important: This must be initialised before use
################################################################################


#' Creates function and static variable used to number charts in report
#' 
#' Must be initialiased, e.g. ggsprint <- save_print_function
#' 
#' @param x Surveyor object
#' @examples
#' ggsprint <<- save_print_function()
save_print_function <- function(x) {
	i <- 1
	f <- function(plot, size=c(4, 3), dpi=600, path) {
		if (x$defaults$output_to_latex && class(plot)=="ggplot") {
			filename <- paste("fig", i, ".eps", sep="")
			message(paste("Now saving ", filename, sep=""))
			ggsave(plot, filename=filename, width=size[1], height=size[2],
					dpi=dpi, path=path)
			cat("\\PlaceGraph{graphics/", filename, "}\n", sep="")
#          cat("\\FloatBarrier","\n",sep="")
			i <<- i + 1
		} else {
			print(plot)
		}
	}
	
	invisible( f )
}


################################################################################
### Plugin architecture to process each question
################################################################################

#' Codes and plots a survey question
#' 
#' This is the top level function that determines how a question is processed,
#' coded, printed and plotted.
#' 
#' @param x Surveyor object
#' @param Qid Question id
#' @param code_function A reference to a function that processes the question data
#' @param plot_function A reference to a function that plots the question data
#' @param plot_size Size in inches of plot output, e.g. c(4,3)
#' @param dpi Print quality in dots per inch 
#' @param plot_path File path where the ggplot graph is saved
#' @param ... Other parameters passed to code_function
#' @export
#' @seealso surveyor
#' @examples
#' plot_q
plot_q <- function(
		surveyor,
		Qid,
		code_function,
		plot_function,
		plot_size=surveyor$defaults$default_plot_size,
		dpi=600,
		plot_path=gpath,
		...){
	
	code_q <- function(
			surveyor,
			Qid,
			code_function,
			...){
		code_function(
				surveyor,
				Qid,
				...)
	}
	
#	if (surveyor$defaults$output_to_latex) printQLatex(get_qtext(surveyor, Qid))
	message(Qid)
	f <- code_q(surveyor, Qid, code_function, ...)
	g <- plot_function(f)
	if (identical(code_function, code_array)) plot_size[2] <- plot_size[2]*1.5
	save_print(plot=g, size=plot_size, dpi=dpi, path=plot_path)
}




#ggprint <- function(x, output_to_latex=FALSE){
#	if (output_to_latex){
#		invisible(x)
#	} else {
#		print(x)
#	}
#}

#ggwindows <- function(x, y, record){
##  windows(x, y, record)
#	invisible(x)
#}


